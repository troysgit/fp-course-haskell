{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ExactlyOne where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- custom data type with a single constructor ExactlyOne that takes type a
data ExactlyOne a = ExactlyOne a deriving (Eq, Show)
-- generally a bad idea to add tyepclasses to a type
 
-- Identity function for ExactlyOne? Takes the constructor and the type a and returns the a
-- runExactlyOne $ ExactlyOne 6
-- We pattern match on the only constructor that there is
runExactlyOne :: ExactlyOne a -> a
runExactlyOne (ExactlyOne a) = a

-- Takes a function of type a and returns type b, takes our custom data type with a and returns 
-- the custom data type with constructor and type b which was returned from the first arg
-- mapExactlyOne (\x -> x*x) (ExactlyOne 6)
mapExactlyOne :: (a -> b) -> ExactlyOne a -> ExactlyOne b
mapExactlyOne f (ExactlyOne x)    = ExactlyOne (f x)


-- Type hole: something waiting to be filled
bindExactlyOne :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
bindExactlyOne     f                    (ExactlyOne a) = f a

instance P.Functor ExactlyOne where
  fmap =
    M.liftM

instance A.Applicative ExactlyOne where
  (<*>) =
    M.ap
  pure =
    ExactlyOne

instance P.Monad ExactlyOne where
  (>>=) =
    flip bindExactlyOne

