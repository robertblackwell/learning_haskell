{-# LANGUAGE DataKinds, KindSignatures #-}
module Sample.State (Cont(..), greetingsFrom) where
import Prelude hiding (Either, Left, Right)
import Data.Char
import Data.List


greetingsFrom :: () -> Prelude.String
greetingsFrom () = "This is module Continuation"



newtype Cont r t = Cont{runCont :: (t -> r) -> r}

type ContF r t = t -> ((t -> r) -> r)

instance Show (Cont r t) where
    show x = "<<Cont>>"

-- 
-- example of continuation passing stype
-- 

squareSimple :: Int -> ((Int -> ()) -> ())
squareSimple x cb = cb $ x * x

cb :: Int -> ()
cb x = ()

cb2 :: Int -> IO()
cb2 x = putStr "Cb2 x : " ++ show x

square :: Int -> (Int -> r) -> r
square x cb = cb $ x * x

plus3 :: Int -> (Int -> r) -> r
plus3 x cb = cb $ x + 3

printInt :: Int -> IO ()
printInt n = putStr $ "This is the value : " ++ show n

-- composition1 :: Int -> ((Int -> r) -> r)
-- composition1 n = result n
--     where
--         f = plus3
--         g = square
--         result :: Int -> ((Int -> r) -> r)
--         result x = f n (\x -> g x)

-- composition :: ContF r t -> ContF r t -> ContF r t
-- composition f g = result
--     where
--         result :: t -> ((t -> r) -> r)
--         result x = f x (g x )

f = plus3
g = square

comp = \x -> f x (\x -> g x)

comp2 x = f x g