module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"

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