{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo: ...") with an appropriate
--   solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

module Course.List where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import Course.Optional
import qualified System.Environment as E
import qualified Prelude as P
import qualified Numeric as N


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap ((P.foldr (:.) Nil) :: ([a] -> List a)) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
data List t = Nil | t :. List t
  deriving (Eq, Ord)

-- This is equivalent to the above
-- data List t = Nil | Cons t (List t)

-- Right-associative
infixr 5 :.

instance Show t => Show (List t) where
  show = show . hlist

-- The list of integers from zero to infinity.
infinity ::
  List Integer
infinity =
  let inf x = x :. inf (x+1)
  in inf 0

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)
{- 
foldRight f b (1 :. (2 :. (3 :. Nil)))
f 1 (foldRight f b (2 :. (3 :. Nil)))
f 1 (f 2 (foldRight f b (3 :. Nil)))
f 1 (f 2 (f 3 (foldRight f b (Nil))))
f 1 (f 2 (f 3 (b)))

add x y = x + y
foldRight add 0 (1 :. (2 :. (3 :. Nil)))
add 1 (foldRight add 0 (2 :. (3 :. Nil)))
add 1 (add 2 (foldRight add 0 (3 :. Nil)))
add 1 (add 2 (add 3 (foldRight add 0 (Nil))))
add 1 (add 2 (add 3 0))
add 1 (add 2 3)
add 1 (5)
6
-}
-- STUDY THIS 
foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

-- foldLeftRight :: (b -> a -> b) -> b -> List a -> b
-- foldLeftRight f b xs = foldRight (\h q -> (\m -> q (f m h))) id xs b

-- foldLeftRight fun start (1 :. 2 :. Nil)
-- foldRight (\h q -> (\m -> q (fun m h))) id (1 :. 2 :. Nil) start
-- (\h q -> (\m -> q (fun m h))) 1 (foldRight (\h q -> (\m -> q (fun m h))) id (2 :. Nil)) start
-- (\q -> (\m -> q (fun m 1))) (foldRight (\h q -> (\m -> q (fun m h))) id (2 :. Nil)) start
-- (\q -> (\m -> q (fun m 1))) ((\h q -> (\m -> q (fun m h))) 2 (foldRight (\h q -> (\m -> q (fun m h))) id Nil) start
-- (\q -> (\m -> q (fun m 1))) ((\q -> (\m -> q (fun m 2))) (foldRight (\h q -> (\m -> q (fun m h))) id Nil) start
-- (\q -> (\m -> q (fun m 1))) ((\q -> (\m -> q (fun m 2)) id) start
-- (\q -> (\m -> q (fun m 1))) (\m -> id (fun m 2)) start
-- (\m -> (\p -> id (fun p 2)) (fun m 1)) start
-- (\p -> id (fun p 2)) (fun start 1)) 
-- id (fun (fun start 1) 2))
-- fun (fun start 1) 2
-- + (+ 0 1) 2 
-- 3
-- This is madness sparta
-- (id . flip fun 2 . flip fun 1) start
-- (id . fun (fun start 1) 2)
-- id (fun (fun start 1) 2)

-- continuation passing; read up on this

foldLeftRight''' :: (b -> a -> b) -> b -> List a -> b
foldLeftRight''' f b xs = foldRight (\h q -> q . flip f h) id xs b
-- be careful with a b c types and how I think about them 

flip' :: (a -> b -> c) -> b -> a -> c
flip' f b a = f a b

foldLeftRight' :: (b -> a -> b) -> b -> List a -> b
foldLeftRight' f b xs = foldRight (\h q m -> q (f m h)) id xs b

{- foldLeftRight f b Nil
foldRight (\h q -> ) id Nil b
id b 
-}
-- this is essentially just for loops
-- foldLeft :: (b -> a -> b) -> b -> List a -> b
-- foldLeft _ b Nil      = b -- return whatever b is 
-- foldLeft f b (h :. t) = foldLeft f (f b h) t
-- foldLeft f b (h :. t) = let b' = f b h in 
  -- b' `seq` foldLeft f b' t

{- some c code 
foldleft f i xs == 
res = i
for x in xs {
  res = f(res, x)
}
return res

so if f is + and init = 0 and xs = [1,2,3]
0
1 
res = 1
3
6

init = 1, f = multiplication, xs = xs again

-}

-- END Helper functions and data types
-- List, infixr, Show, infinity, foldRight, foldLeft


-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> \x -> x `headOr` infinity == 0
--
-- prop> \x -> x `headOr` Nil == x
headOr :: a -> List a -> a
headOr = 
  foldRight const -- const always evaluates to its first argument, ignoring the second
{-

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

const x y = x
foldRight const -10 (1 :. 2 :. 3 :. Nil)
      x  y
const 1 (foldRight const -10 (2 :. 3 :. Nil))
1

foldRight const -10 Nil
-10

-}
-- | The product of the elements of a list.
--
-- >>> product Nil
-- 1
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
{-
foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = foldLeft f (f b h) t
foldLeft f b (h :. t) = 
  let b' = f b h 
  in b' `seq` foldLeft f b' t

foldLeft f b (h :. t) = 
  in f b h `seq` foldLeft f (f b h) t

-- equivalent to the let above 
foldLeft f b (h :. t) = b' `seq` foldLeft f b' t
  where b' = f b h 
  
product (1 :. 2 :. 3 :. Nil)
foldLeft (*) 1 (1 :. 2 :. 3 :. Nil)
foldLeft (*) ((*) 1 1) (2 :. 3 :. Nil)
foldLeft (*) 1 (2 :. 3 :. Nil)
foldLeft (*) ((*) 1 2) (3 :. Nil)
foldLeft (*) 2 (3 :. Nil)
foldLeft (*) ((*) 2 3) Nil
foldLeft (*) 6 Nil
6
-}

 
product :: List Int
  -> Int
product = foldLeft (*) 1 -- this works but I don't think I understand how it's taking a list as an argument
-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- EVAL BY HAND

-- prop> \x -> foldLeft (-) (sum x) x == 0
sum ::
  List Integer
  -> Integer
sum = foldLeft (+) 0 -- in this instance foldLeft vs foldRight is inconsequential? Until we work with folding
-- over a range? 
  
-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- EVAL BY HAND
-- prop> \x -> sum (map (const 1) x) == length x
length :: List a
  -> Int
length = foldRight (\_ cnt -> cnt + 1) 0

-- | Map the given function on each element of the list.
--

-- EVAL BY HAND
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> \x -> headOr x (map (+1) infinity) == 1
--
-- prop> \x -> map id x == x
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (h :. t) = f h :. map f t
-- Next week: translate between the above and a shorter foldRight below
-- map f = foldRight (\val xs -> f val :. xs) Nil

    -- [f x | x <- xs] -- this no work. Syntactic sugar. 
  
-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> \x -> headOr x (filter (const True) infinity) == 0
--
-- prop> \x -> filter (const True) x == x
--
-- prop> \x -> filter (const False) x == Nil
-- Looks a lot like map, nil and cons clause BUT the cons case is going to be more complicated (IF statement)
--- if then else 
filter :: (a -> Bool) -> List a
  -> List a
filter _ Nil = Nil
filter f (x :. xs) = if f x then x :. filter f xs else filter f xs
-- 
-- filter f (x:.xs) = if f x == True then x :. ys else Nil
--   where ys = () 

-- | Append two lists to a new list.
--
-- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> \x -> headOr x (Nil ++ infinity) == 0
--
-- prop> \x -> headOr x (y ++ infinity) == headOr 0 y
--
-- prop> \x -> (x ++ y) ++ z == x ++ (y ++ z)
--
-- prop> \x -> x ++ Nil == x
-- I need to think about this one some more
(++) :: List a -> List a -> List a
xs ++ ys = foldRight (:.) ys xs 
-- (++) Nil Nil = Nil
-- (++) (xs) Nil = xs
-- (++) Nil (ys) = ys
-- -- (++) (x :. xs) ys = if x == Nil then ys else xs ++ ys
-- (++) (x :. xs) ys = x :. (xs ++ ys)

infixr 5 ++

-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> \x -> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> sum (map length x) == length (flatten x)
flatten :: List (List a)
  -> List a
flatten =
  foldRight (++) Nil 
-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> flatMap id (x :: List (List Int)) == flatten x
flatMap ::
  (a -> List b)
  -> List a
  -> List b
flatMap f xs = foldRight (\x acc -> f x ++ acc) Nil xs
-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> \x -> let types = x :: List (List Int) in flatten x == flattenAgain x

-- **** Scotty (for newbs) for Haskell Web starting point. Servant for type level pros.  
-- ******* start with flatmap _ xs and see what the compiler tells re. type of function
-- flattenAgain :: List (List a)
--   -> List a
-- flattenAgain xs = flatMap id xs-- HELP 

-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values,
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1, 10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
-- seqOptional :: List (Optional a)
--   -> Optional (List a)
-- seqOptional xxs 
-- seqOptional (Empty:_) = Empty
-- seqOptional (Full x:xs) = do
--   rem <- seqOptional xs
--   return (x:rem)

  -- flattenAgain x  
  -- seqOptional xs 
  -- error "todo: Course.List#seqOptional"

-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
find ::
  (a -> Bool)
  -> List a
  -> Optional a
find f (x :. xs) = if f x then Full x else find f xs
find _ Nil = Empty

  -- error "todo: Course.List#find"

-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
lengthGT4 ::
  List a
  -> Bool
-- lengthGT4 xs = if find (\x acc -> acc+1) > 4 then True else 
lengthGT4 xs = foldRight (\_ acc -> acc + 1) 0 xs > 4 
-- lengthGT4 infinity = True

  -- error "todo: Course.List#lengthGT4"

-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- >>> take 1 (reverse (reverse largeList))
-- [1]
--
-- prop> \x -> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse ::
  List a
  -> List a
reverse = 
  foldLeft (flip (:.)) Nil
 -- error "todo: Course.List#reverse"

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce ::
  (a -> a)
  -> a
  -> List a
produce f x = x :. produce f (f x)
  -- error "todo: Course.List#produce"

-- | Do anything other than reverse a list.
-- Is it even possible?
--
-- >>> notReverse Nil
-- []
--
-- prop> \x y -> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
notReverse ::
  List a
  -> List a
notReverse = id 
  -- error "todo: Is it even possible?"

---- End of list exercises

largeList ::
  List Int
largeList =
  listh [1..50000]

hlist ::
  List a
  -> [a]
hlist =
  foldRight (:) []

listh ::
  [a]
  -> List a
listh =
  P.foldr (:.) Nil

putStr ::
  Chars
  -> IO ()
putStr =
  P.putStr . hlist

putStrLn ::
  Chars
  -> IO ()
putStrLn =
  P.putStrLn . hlist

readFile ::
  FilePath
  -> IO Chars
readFile =
  P.fmap listh . P.readFile . hlist

writeFile ::
  FilePath
  -> Chars
  -> IO ()
writeFile n s =
  P.writeFile (hlist n) (hlist s)

getLine ::
  IO Chars
getLine =
  P.fmap listh P.getLine

getArgs ::
  IO (List Chars)
getArgs =
  P.fmap (listh . P.fmap listh) E.getArgs

isPrefixOf ::
  Eq a =>
  List a
  -> List a
  -> Bool
isPrefixOf Nil _ =
  True
isPrefixOf _  Nil =
  False
isPrefixOf (x:.xs) (y:.ys) =
  x == y && isPrefixOf xs ys

isEmpty ::
  List a
  -> Bool
isEmpty Nil =
  True
isEmpty (_:._) =
  False

span ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
span p x =
  (takeWhile p x, dropWhile p x)

break ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
break p =
  span (not . p)

dropWhile ::
  (a -> Bool)
  -> List a
  -> List a
dropWhile _ Nil =
  Nil
dropWhile p xs@(x:.xs') =
  if p x
    then
      dropWhile p xs'
    else
      xs

takeWhile ::
  (a -> Bool)
  -> List a
  -> List a
takeWhile _ Nil =
  Nil
takeWhile p (x:.xs) =
  if p x
    then
      x :. takeWhile p xs
    else
      Nil

zip ::
  List a
  -> List b
  -> List (a, b)
zip =
  zipWith (,)

zipWith ::
  (a -> b -> c)
  -> List a
  -> List b
  -> List c
zipWith f (a:.as) (b:.bs) =
  f a b :. zipWith f as bs
zipWith _ _  _ =
  Nil

unfoldr ::
  (a -> Optional (b, a))
  -> a
  -> List b
unfoldr f a  =
  case f a of
    Full (b, a') -> b :. unfoldr f a'
    Empty -> Nil

lines ::
  Chars
  -> List Chars
lines =
  listh . P.fmap listh . P.lines . hlist

unlines ::
  List Chars
  -> Chars
unlines =
  listh . P.unlines . hlist . map hlist

words ::
  Chars
  -> List Chars
words =
  listh . P.fmap listh . P.words . hlist

unwords ::
  List Chars
  -> Chars
unwords =
  listh . P.unwords . hlist . map hlist

listOptional ::
  (a -> Optional b)
  -> List a
  -> List b
listOptional _ Nil =
  Nil
listOptional f (h:.t) =
  let r = listOptional f t
  in case f h of
       Empty -> r
       Full q -> q :. r

any ::
  (a -> Bool)
  -> List a
  -> Bool
any p =
  foldRight ((||) . p) False

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight ((&&) . p) True

or ::
  List Bool
  -> Bool
or =
  any id

and ::
  List Bool
  -> Bool
and =
  all id

elem ::
  Eq a =>
  a
  -> List a
  -> Bool
elem x =
  any (== x)

notElem ::
  Eq a =>
  a
  -> List a
  -> Bool
notElem x =
  all (/= x)

permutations
  :: List a -> List (List a)
permutations xs0 =
  let perms Nil _ =
        Nil
      perms (t:.ts) is =
        let interleave' _ Nil r =
              (ts, r)
            interleave' f (y:.ys) r =
               let (us,zs) = interleave' (f . (y:.)) ys r
               in  (y:.us, f (t:.y:.us):.zs)
        in foldRight (\xs -> snd . interleave' id xs) (perms ts (t:.is)) (permutations is)
  in xs0 :. perms xs0 Nil

intersectBy ::
  (a -> b -> Bool)
  -> List a
  -> List b
  -> List a
intersectBy e xs ys =
  filter (\x -> any (e x) ys) xs

take ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
take n _  | n <= 0 =
  Nil
take _ Nil =
  Nil
take n (x:.xs) =
  x :. take (n - 1) xs

drop ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
drop n xs | n <= 0 =
  xs
drop _ Nil =
  Nil
drop n (_:.xs) =
  drop (n-1) xs

repeat ::
  a
  -> List a
repeat x =
  x :. repeat x

replicate ::
  (Num n, Ord n) =>
  n
  -> a
  -> List a
replicate n x =
  take n (repeat x)

reads ::
  P.Read a =>
  Chars
  -> Optional (a, Chars)
reads s =
  case P.reads (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

read ::
  P.Read a =>
  Chars
  -> Optional a
read =
  mapOptional fst . reads

readHexs ::
  (Eq a, Num a) =>
  Chars
  -> Optional (a, Chars)
readHexs s =
  case N.readHex (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readHex ::
  (Eq a, Num a) =>
  Chars
  -> Optional a
readHex =
  mapOptional fst . readHexs

readFloats ::
  (RealFrac a) =>
  Chars
  -> Optional (a, Chars)
readFloats s =
  case N.readSigned N.readFloat (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readFloat ::
  (RealFrac a) =>
  Chars
  -> Optional a
readFloat =
  mapOptional fst . readFloats

instance (a ~ Char) => IsString (List a) where
  -- Per https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.String.html#line-43
  fromString =
    listh

type Chars =
  List Char

type FilePath =
  List Char

strconcat ::
  [Chars]
  -> P.String
strconcat =
  P.concatMap hlist


stringconcat ::
  [P.String]
  -> P.String
stringconcat =
  P.concat

show' ::
  Show a =>
  a
  -> List Char
show' =
  listh . show

instance P.Functor List where
  fmap f =
    listh . P.fmap f . hlist

instance A.Applicative List where
  (<*>) =
    M.ap
  pure =
    (:. Nil)

instance P.Monad List where
  (>>=) =
    flip flatMap
