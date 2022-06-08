-- module Main where
-- simple type class
-- 
-- Moving on, lets do another example. This time a little more extensive.
-- The topic A return type that carries accumulated error messages.
-- 
{-# LANGUAGE DataKinds, KindSignatures #-}
module Sample.Trace (Trace(..), greetingsFrom) where 
import Prelude hiding (Either, Left, Right)
import Data.Char
import Data.List


greetingsFrom :: () -> Prelude.String
greetingsFrom () = "This is module Trace"

-- Either is a bit like our  Trace monad except that the "Error" piece is allowed to be any type (with some restrictions)
-- However like the  Trace the two sides are not equal in the way they area treasted



data Trace (a :: Show) = Trace{value :: a, messages :: [String] } deriving(Show)

-- now for a given `left` Trace left is going to be a monad



lift :: (a -> b -> c) -> (Trace a -> Trace b -> Trace c)
lift op xa xb = Trace{value = opresult, messages = mergedMessages}
    where
        vxa = value xa
        vxb = value xb
        opresult = op vxa vxb 
        ma = messages xa
        mb = messages xb
        mm = ma ++ mb 
        new_message = "function called : " ++ show vxa ++ " result " ++ show (opresult)
        mergedMessages = new_message : mm


-- lift op _ (LeftF lv) = LeftF lv
-- lift op (RightF a) (RightF b) = RightF (op a b) 

-- instance Prelude.Functor (Trace left) where
--     fmap _ (LeftF lv) = LeftF lv
--     fmap k (RightF x) = RightF (k x)

-- instance Prelude.Applicative (Trace left) where
--     pure x = RightF x
--     (<*>)  = lift Prelude.id

-- -- now lets make it a monad

-- eta :: a -> Trace left a
-- eta  = RightF 

-- mu :: Trace left (Trace left a) -> Trace left a
-- mu (LeftF lv)          = LeftF lv
-- mu (RightF (LeftF lv)) = LeftF lv
-- mu (RightF (RightF x)) = RightF x


-- -- lets make  Trace a monad
-- -- now the kliesliLift lifting of a function
-- -- this is a general calculation not specific to our particular Functor
-- kliesliLift :: (a ->  Trace left b) -> ( Trace left a ->  Trace left b)
-- -- The . in the next statement is function composition fromPrelude
-- kliesliLift k = mu . Prelude.fmap k

-- -- demonstrate the kliesliLift composition >==>
-- -- this is a general calculation not specific to our particular Functor
-- kliesliComposition :: (a ->  Trace left b) -> (b ->  Trace left c) -> ( Trace left a ->  Trace left c)
-- kliesliComposition f g = kliesliLift g . kliesliLift f

-- -- this is a general calculation not specific to our particular Functor
-- (>=>) :: (a ->  Trace left b) -> (b ->  Trace left c) -> ( Trace left a ->  Trace left c)
-- (>=>) = kliesliComposition

-- -- and from that a definition of the bind operator
-- -- this is a general calculation not specific to our particular Functor
-- bind ::  Trace left a -> (a ->  Trace left b) ->  Trace left b
-- bind fa k = kliesliLift k fa  

-- -- this one is not correct
-- -- this is a general calculation not specific to our particular Functor
-- thenFunction ::  Trace left a ->  Trace left b ->  Trace left b
-- thenFunction fa fb = bind fa (\_ -> fb) 


-- -- thenf Failed _ = Failed
-- -- thenf _ Failed = Failed 
-- -- thenf (Ok g) (Ok fb) = Ok fb

-- -- and finally make our functor a Monad
-- -- this is a general calculation not specific to our particular Functor
-- -- at least given that we have eta amd mu
-- instance Prelude.Monad  (Trace left) where
--     -- return x = Ok x
--     return x = eta x
--     (>>=) = bind
--     -- (>>=) (Ok x) g = g x
--     -- (>>=) Failed g = Failed
--     (>>) = thenFunction
