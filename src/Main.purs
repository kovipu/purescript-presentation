module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy, (<|>))
import Parsing.Combinators.Array (many)
import Parsing.String (anyTill, char, string)
import Parsing.String.Basic (intDecimal, space)

main :: Effect Unit
main = do
  let res = runParser input (parseRule `sepBy` (char '\n'))
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
input = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"""

type Rule =
  { container :: String
  , content :: Array Content
  }

type Content =
  { color :: String
  , number :: Int
  }

parseRule :: Parser String Rule
parseRule = do
  container <- parseColor
  _ <- string "bags contain "
  content <- many parseContent
  pure { container, content }
  where
    parseColor = do
      prefix <- parseString
      color <- parseString
      pure $ prefix <> " " <> color
    
    parseContent = do
      number <- intDecimal
      _ <- space
      color <- parseColor
      _ <- string "bags" <|> string "bag"
      _ <- string ", " <|> string "."
      pure { color, number }
    
    parseString = do
      (Tuple s _) <- anyTill space
      pure $ s
