# Bake your typeclasses
## Hedgehog

The most obvious use case for this library is hedgehog generators. In
quickcheck, you have `Arbitrary` typeclasses, but they're very hard to replace
in case you need something slightly different than what the original author had
in mind for construction or shrinking. Hedgehog solves this problem by going
full value-level construction - which is explicit, but verbose. Even with
Hedgehog, it's hard to replace part of a generator without explicitly making it
possible to pass in a generator for a type used in a generator.

```haskell
newtype Name = Name Text
newtype Email = Email Text

data Person = Person
  { _name :: Name
  , _email :: Email
  }

data Company = Company
  { _employees :: [Person]
  }
```

The standard way for generating a `Company` object would be to write a generator
for `Name`, one for `Email`, and then use the two to write one for `Company`.

### Hegehog Generators

```haskell
{-# LANGUAGE ApplicativeDo #-} -- because I'm lazy

genName = Name <$> text (linear 3 20) unicode
genEmail = Email <$> do
  user <- text (linear 3 20) ascii
  host <- text (linear 3 10) ascii
  pure $ Email $ (user <> "@" <> host)

genericTransientRecipeInstance ''Person

genCompany = do
  employees <- Gen.list (linear 3 10) genPerson
```

There are a few problems with this approach. If you want to change how an email
is created (e.g. all should have the same, company-provided domain), you'll have
to rewrite `genPerson` and `genCompany` to accept `genEmail` as an argument. If
you want full extensibility, you'll end up with a sea of parameters.

### Cookable Generators

This library allows you to implement default constructors for types, and
override them as required. The above code example will look like - full code in
[HedgehogExample.hs](src/HedgehogExample).

```haskell
instance MonadGen m => DefaultRecipe Identity (m Name) where
  type DefaultRecipeDeps Identity (m Name) = '[]
  def = pureRecipe $ Name <$> text (linear 3 20) unicode

instance MonadGen m => DefaultRecipe Identity (m Email) where
  type DefaultRecipeDeps Identity (m Email) = '[]
  def = pureRecipe $ do
    user <- text (linear 3 20) ascii
    host <- text (linear 3 10) ascii
    pure $ Email $ (user <> "@" <> host)

instance MonadGen m => DefaultRecipe Identity (m Person) where
  type DefaultRecipeDeps Identity (m Person) = '[m Name, m Email]
  def = Recipe $ \deps -> pure $ do
    name <- grab deps
    email <- grab deps
    pure $ Person name email

instance MonadGen m => DefaultRecipe Identity (m Company) where
  type DefaultRecipeDeps Identity (m Company) = '[m Person]
  def = Recipe $ \deps -> pure $ do
    employees <- Gen.list (linear 3 10) (grab deps)
    pure $ Company employees
```

### Anatomy of a DefaultRecipe

```haskell
instance MonadGen m => DefaultRecipe Identity (m Person) where
  type DefaultRecipeDeps Identity (m Person) = '[m Name, m Email]
  def = Recipe $ \deps -> pure $ do
    name <- grab deps
    email <- grab deps
    pure $ Person name email
```

The `Identity` Monad is used if construcing an ingredient doesn't incur a
sideeffect. The next argument to the typeclass is the thing being constructed.

```haskell
instance MonadGen m => DefaultRecipe Identity (m Person) where
```

`DefaultRecipeDeps` is the associated type which annotates which other
ingredients are required to cook a recipe. The type is a type-level list, hence
the '.

```haskell
  type DefaultRecipeDeps Identity (m Person) = '[m Name, m Email]
```

`def` is a `Recipe`. Inside the `Recipe`, you can fetch the dependencies by
using `grab` from data-diverse. Type annotation is usually not requried, because
GHC can figure it out for you. If you forgot to annotate dependencies, you'll
get an error message about the element not being in `deps`.

```haskell
  def = Recipe $ \deps -> pure $ do
    name <- grab deps
    email <- grab deps
    pure $ Person name email
```

### Overriding Dependencies

For providing modified `Recipes`, pass it as a `Many` to the `finish` function.
The `@m` is required for now, until #5 is fixed.

```
largeCompanyGen' :: forall (m :: * -> *). MonadGen m => Recipe Identity (m Company) '[m Person]
largeCompanyGen' = Recipe $ \deps -> pure $ do
  employees <- Gen.list (linear 100 1000) (grab deps)
  pure $ Company employees

largeCompanyGen :: forall m. MonadGen m => (m Company)
largeCompanyGen = runIdentity $ finish (largeCompanyGen' @m ./ nil)
```

The library checks if a recipe actually overrides anything in the cooking
process. If it doesn't you'll get an error. These can mean that a) the effect
types don't match up (#6) b) the type isn't required by any of your recipes.

```
The type Recipe Identity M5 '[M0] is not overriding anything in '[M0]
```

## Cooking explained

The basic element of this library is the Recipe. It represent a function from
its dependencies (represented as an hlist) to a target inside an effect.

```haskell
newtype Recipe (effect :: Type -> Type) target (deps :: [Type]) =
  Recipe { runRecipe :: forall depStore. HasTypes deps depStore => depStore -> effect target }
```

You can cook a target by supplying a book of `Recipe`s to the `finish`
function, without any default typeclasses.

```haskell
newtype M1 = M1 ()
newtype M2 = M2 M1

r1 :: Recipe Identity M1 '[]
r1 = pureRecipe $ M1 ()

r2 :: Recipe Identity M2 '[M1]
r2 = Recipe $ \deps -> pure $ M2 $ grab deps

c2 :: M2
c2  = runIdentity $ finish (r1 ./ r2 ./ nil)
```

For convenience, it's possible to deposit default recipes as typeclasses.

```haskell
instance DefaultRecipe Identity M1 where
  type DefaultRecipeDeps = '[]
  def = r1
```

If a required type does not exist in the book passed to `finish`, the compiler
will try to find a corresponding `DefaultRecipe`. The replacement also goes the
other way, where you can replace default implementations with values passed to
`finish`.

# Acknowledgements

Initial idea by @etorreborre (for a module system, and the idea to store
dependencies via Maybe), with a lot of help with the typelevel programming from
Alejandro Mena.
