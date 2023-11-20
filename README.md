# purescript-presentation

### 0. Initialize a project

```shell
mkdir presentation
npm i -g purescript spago
spago init
```

Show around the boilerplate and run it.

```shell
spago run
```

### 1. Basics of the syntax

Let's define a function.
There can be any number of parameters delimited by spaces.

```purescript
add1 x = x + 1
```

Show running it in repl.

```shell
spago repl
import Main
add1 3
:type add1
```

In PureScript, most everything is a function.
Constants use the same syntax as functions but without parameters.

PureScript's types support type inference: far fewer explicit types are required than most languages.
The type system is a tool rather than a hindrance.
While most of the time type annotations are not required, the developer can choose to use them for documentation.

```purescript
add1 :: Int -> Int
add1 x = x + 1
```

Type inference always infers the most generic type possible.

```purescript
duplicate x = [x, x]
```

For this function, `:type` infers us a type signature with `forall t1`, basically the same as TypeScript's `any` type.
Except in this case it is the correct type here, and not just the developer being lazy.

We can add this type signature.

```purescript
duplicate :: forall a. a -> Array a
duplicate x = [x, x]
```

Let's create another function using generic type parameters.

```purescript
showAndDup x = [show x, show x]
```

Here, type inference gives us the the most generic type using the Show type class.
The Show type class means that the function accepts any type that implements the function `show :: a -> String`.

This is the simplest example of a type class in use, but there's similar type classes for a lot more things: for example JSON-serialization.
And you can create your own as well.

Let's add this type signature to our program.

```purescript
showAndDup :: forall a. Show a => a -> Array String
showAndDup x = [show x, show x]
```

### 2. Modeling data with PureScript's type system

Let's consider modeling routes of our application.
We could use a `String` for this, but not all `String`s are valid routes.
Instead, we can use an ADT (= Algebraic Data Type) to model only valid routes.

```purescript
data Route
  = Home
  | Settings

route :: Route
route = Home
```

This is different from things like enums in that the value of the data is the word itself, not some other value like an integer.

ADTs can also be defined to include data along with the type constructor.
For example, in PureScript nullable state is modeled using the Maybe type.

```purescript
data MaybeInt
  = Nothing
  | Just Int
```

Other generic ADTs are Either, Tuple & RemoteData.

ADTs are also super useful for modeling domain data.
Consider: user of our application is required atleast one contact info type of: email or phone.
ADTs allow us to model this and make the illegal states impossible to represent.

```purescript
newtype Email = Email String
newtype Phone = Phone String

-- wrong: user should be able to have both. Not permissive enough!
type ContactInfo = Either Email Phone

-- wrong: user can be constructed with neither. Too permissive!
type ContactInfo = Tuple (Maybe Email) (Maybe Phone)

-- correct: only valid data can be constructed.
data ContactInfo
  = EmailOnly Email
  | PhoneOnly Phone
  | Both Email Phone
```

PureScript supports pattern matching, which is an important tool when working with ADTs.
You can pattern match at the function definition level:

```purescript
handleContact :: ContactInfo -> Int
handleContact (EmailOnly e) = 1
handleContact (PhoneOnly p) = 2
handleContact (Both e p) = 3
```

Or inside a function using a case expression:

```purescript
handleContact' :: ContactInfo -> Int
handleContact' contactInfo =
  case contactInfo of
    EmailOnly e -> 1
    PhoneOnly p -> 2
    Both e p -> 3
```

One more thing before we move to the next section.
Instead of using pattern matchin with Maybe:

```purescript
handleMaybeInt :: Maybe Int -> Maybe Int
handleMaybeInt m =
  case m of
    Nothing -> Nothing
    Just i -> Just $ add1 i
```

You can use the bind-operation, because the Maybe type implements the Bind type class.

```purescript
handleMaybeInt' :: Maybe Int -> Maybe Int
handleMaybeInt' m =
  m >>= (add1 >>> Just)
  -- same as (\i -> Just $ add1 i)
```

PureScript has syntactic sugar for the bind operation, called the do-block.

```purescript
handleMaybeInt'' :: Maybe Int -> Maybe Int
handleMaybeInt'' m = do
  i <- m
  Just $ add1 i
```

This allows us to write this same code, but make it look imperative.
This will become important a bit later in the presentation.
I personally find this more readable.

Maybe and other similar types that have a happy path and an error/waiting/etc path implement the Bind type class.
For example, Either & RemoteData.

ADTs are the language feature I miss the most in languages that don't have them.
TypeScript's tagged unions are a clunky solution for the problem.

### 3. Interfacing with JavaScript code

PureScript has a powerful FFI (Foreign Function Interface) for calling JavaScipt code.
This is super important for being a productive real world language: no need to throw away or rewrite existing code.

That being said, interfacing with JavaScript is dangerous and throws away a lot of the safety of PureScript.
Best practice is to use PureScript instead of JavaScript as much as possible.

Let's create a new JavaScript file: Main.js.

```js
export const compute = (n) => {
  return n + 4;
};
```

This can be imported into PureScript with foreign import:

```purescript
foreign import compute :: Int -> Int
```

Works as it should in the REPL.
Now let's see how writing effectful JavaScript works.

```js
export const computeWithSideEffects = (n) => {
  const rand = Math.floor(Math.random() * 10);
  return n + rand;
};
```

```purs
foreign import computeWithSideEffects :: Int -> Int
```

This works in the REPL, but it isn't pure!
The PureScript compiler has no power to limit effects in the JS code.
To communicate to the compiler that this is effectful code, we use a function with zero arguments in the JS code.

```js
export const computeWithSideEffects_ = (n) => () => {
  const rand = Math.floor(Math.random() * 10);
  return n + rand;
};
```

```purs
foreign import computeWithSideEffects_ :: Int -> Effect Int
```

There's more to this, like how to call type class member functions, using more complex types, and converting JS-objects to PureScript data with strong safety.
But this is the basics of it.
Quite simple to use compared to something like Elm's port system.

### 4. Parsing with purescript parsing

Open [Advent of Code 2020 day 2](https://adventofcode.com/2020/day/2).
Copy & paste that example rule into a string constant.

```purs
input :: String
input = """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
"""
```

Write the structured data type to model this.

```purs
type Password =
  { min :: Int
  , max :: Int
  , char :: Char
  , password :: String
  }
```

We could use a stricter type for the password than String.
I'm deciding to be lazy.

Let's write a parser to parse this data.

```purs
parsePassword :: Parser String Password
parsePassword = do
  min <- intDecimal
  _ <- char '-'
  max <- intDecimal
  _ <- space
  char <- anyChar
  _ <- string ": "
  Tuple password _ <- (anyTill $ string "\n")
  -- same as: password <- fst <$> (anyTill $ string "\n")
  pure { min, max, char, password }
```

Update the main-function to run this parser.

```purs
main :: Effect Unit
main = do
  let res = runParser input $ many parsePassword
  log $ show res
```

Works with `spago run`.
Now draw rest of the fucking owl and solve the problem.
This sort of parsing is amazing for Advent of Code, try and find a similar library for your weapon of choice.
