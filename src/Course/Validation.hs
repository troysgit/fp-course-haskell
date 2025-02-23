{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Validation where

import qualified Prelude as P(String)
import Course.Core
 -- triple arrows usually something around doctests
-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap, either)
-- >>> instance Arbitrary a => Arbitrary (Validation a) where arbitrary = P.fmap (P.either Error Value) arbitrary
data Validation a = Error Err | Value a
  deriving (Eq, Show)

type Err = P.String

-- | Returns whether or not the given validation is an error.
--
-- >>> isError (Error "message")
-- True
--
-- >>> isError (Value 7)
-- False
--
-- prop> \x -> isError x /= isValue x
-- the following is basically syntax sugar
isError :: Validation a -> Bool
isError (Error _) = True
isError (Value _) = False


-- Different syntax
isErrorAlt :: Validation a -> Bool 
isErrorAlt v = case v of
  Error _ -> True
  Value _ -> False
   
-- | Returns whether or not the given validation is a value.
--
-- >>> isValue (Error "message")
-- False
--
-- >>> isValue (Value 7)
-- True
--
-- prop> \x -> isValue x /= isError x
-- a Value of type Validation a 
-- isValue is the composition of not and isError
isValue :: Validation a -> Bool
isValue = not . isError

-- pointful version of the above
isValue' :: Validation a -> Bool
isValue' v = not (isError v)

-- lambda. Definition of composition but expanded. 
isValueAlt :: Validation a -> Bool 
isValueAlt = (\v -> not (isError v))


-- | Maps a function on a validation's value side.
--
-- >>> mapValidation (+10) (Error "message")
-- Error "message"
--
-- >>> mapValidation (+10) (Value 7)
-- Value 17
--
-- prop> \x -> mapValidation id x == x
mapValidation :: (a -> b) -> Validation a -> Validation b
mapValidation _ (Error s) = Error s
mapValidation f (Value a) = Value (f a)

-- | Binds a function on a validation's value side to a new validation.
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Error "message")
-- Error "message"
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 7)
-- Error "odd"
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 8)
-- Value 18
--
-- prop> \x -> bindValidation Value x == x
bindValidation :: (a -> Validation b) -> Validation a -> Validation b
bindValidation _ (Error s) = Error s
bindValidation f (Value a) = f a
--bindValidation f (Value a) = Error "Oh No"

-- | Returns a validation's value side or the given default if it is an error.
--
-- >>> valueOr (Error "message") 3
-- 3
--
-- >>> valueOr (Value 7) 3
-- 7
--
-- prop> \x -> isValue x || valueOr x n == n
valueOr :: Validation a -> a -> a
valueOr (Error _) def = def
valueOr (Value x) _ = x

-- | Returns a validation's error side or the given default if it is a value.
--
-- >>> errorOr (Error "message") "q"
-- "message"
--
-- >>> errorOr (Value 7) "q"
-- "q"
--
-- quickcheck tests: prop == proposition
-- prop> \x -> isError x || errorOr x e == e
errorOr :: Validation a -> Err -> Err
errorOr (Error e) _ = e
errorOr (Value _) a = a

valueValidation :: a -> Validation a
valueValidation = Value
