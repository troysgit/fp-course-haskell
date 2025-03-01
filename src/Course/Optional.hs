{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a = 
    Full a 
  | Empty
  deriving (Eq, Show)

-- data Optional' a
-- | Return the possible value if it exists; otherwise, the first argument.
--
-- >>> fullOr 99 (Full 8)
-- 8
--
-- >>> fullOr 99 Empty
-- 99
fullOr :: a -> Optional a -> a
fullOr defA optA =
  case optA of
    Full a -> a
    Empty -> defA

fullOr' :: a -> Optional a -> a
fullOr' defA Empty = defA
fullOr' _ (Full optA) = optA

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
-- mapOptional :: (a -> b) -> Optional a -> Optional b
-- mapOptional f potentA = 
--   case potentA of 
--     Full x -> Full (f x) 
--     Empty -> Empty 

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional f (Full potentA) = Full (f potentA)
mapOptional _ Empty = Empty

  -- | Full potentA = f (Full potentA)
  -- | Empty = Empty
  -- case potentA of 
  --   Full x -> f x
  --   Empty -> _ 


-- mapOptional' :: (a -> b) -> Optional a -> Optional b
-- mapOptional' f potentA
--   | if Full potentA then f potentA else potentA = Empty
--   | otherwise = Empty

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional f Empty = Empty 
bindOptional f (Full a) = f a

bindOptional' :: (a -> Optional b) -> Optional a -> Optional b
bindOptional' f (Full potentA) = f potentA
bindOptional' _ Empty = Empty


bindOptionalExposition :: (a -> Optional b) -> Optional a -> Optional b
bindOptionalExposition = (\f -> \optA -> case optA of
    Empty -> Empty
    Full x  -> f x)

-- These are all equivalent
bindOptionalExposition' :: (a -> Optional b) -> Optional a -> Optional b
bindOptionalExposition' f = \optA -> case optA of
    Empty -> Empty
    Full x  -> f x

bindOptionalExpositionTwo :: (a -> Optional b) -> Optional a -> Optional b
bindOptionalExpositionTwo f optA = case optA of
    Empty -> Empty
    Full x  -> f x
    
-- bindPartial :: (a -> Optional b) -> Optional a
-- bindPartial f a = (Full a)

myAdd :: (Num a) => a -> a -> a 
myAdd inp inp2 = inp + inp2

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) :: Optional a -> Optional a -> Optional a
(<+>) a b =
  case a of 
    Full _ -> a
    _ -> b

arrowPlusArrow :: Optional a -> Optional a -> Optional a
arrowPlusArrow (Full a) _ = Full a
arrowPlusArrow _ b = b


-- (<+>) (Full a) _ = a
-- (<+>) _  (Full b) = b

-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0

-- optional' :: (a -> b) -> Optional a -> b
-- optional' _ Empty = _
-- optional' f (Full a) = f a  

optional :: (a -> b) -> b -> Optional a -> b
optional f x Empty = x
optional f x (Full optY) = f optY

applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
