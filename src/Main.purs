module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String (anyChar, anyTill, char, string)
import Parsing.String.Basic (intDecimal, space)

main :: Effect Unit
main = do
  let res = runParser input $ many parsePassword
  log $ show res

-- 1. Basics of the syntax

add1 :: Int -> Int
add1 x = x + 1

duplicate :: forall a. a -> Array a
duplicate x = [x, x]

showAndDup :: forall a. Show a => a -> Array String
showAndDup x = [show x, show x]

-- 2. Modeling data with PureScript's type system

data Route
  = Home
  | Settings

route :: Route
route = Home

newtype Email = Email String
newtype Phone = Phone String

data ContactInfo
  = EmailOnly Email
  | PhoneOnly Phone
  | Both Email Phone

handleContact :: ContactInfo -> Int
handleContact (EmailOnly e) = 1
handleContact (PhoneOnly p) = 2
handleContact (Both e p) = 3

handleContact' :: ContactInfo -> Int
handleContact' contactInfo =
  case contactInfo of
    EmailOnly e -> 1
    PhoneOnly p -> 2
    Both e p -> 3

handleMaybeInt :: Maybe Int -> Maybe Int
handleMaybeInt m =
  case m of
    Nothing -> Nothing
    Just i -> Just $ add1 i

handleMaybeInt' :: Maybe Int -> Maybe Int
handleMaybeInt' m =
  m >>= add1 >>> Just

handleMaybeInt'' :: Maybe Int -> Maybe Int
handleMaybeInt'' m = do
  i <- m
  Just $ add1 i

-- 3. Interfacing with JavaScript code
foreign import compute :: Int -> Int

foreign import computeWithSideEffects :: Int -> Int

foreign import computeWithSideEffects_ :: Int -> Effect Int

-- 4. Parsing with purescript-parsing

input :: String
input = """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
"""

type Password =
  { min :: Int
  , max :: Int
  , char :: Char
  , password :: String
  }

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
