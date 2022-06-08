-- module Main where
-- simple type class
-- 
-- Moving on, lets do another example. This time a little more extensive.
-- The topic A return type that carries accumulated error messages.
-- 
module Sample.Either (EitherF(..), greetingsFrom) where 
import Prelude hiding (Either, Left, Right)
import Data.Char
import Data.List


greetingsFrom :: () -> Prelude.String
greetingsFrom () = "This is module Either"

-- Either is a bit like our  EitherF monad except that the "Error" piece is allowed to be any type (with some restrictions)
-- However like the  EitherF the two sides are not equal in the way they area treasted



data EitherF left right = LeftF left | RightF right  deriving(Show)


-- now for a given `left` EitherF left is going to be a monad



lift :: (a -> b -> c) -> (EitherF left a -> EitherF left b -> EitherF left c)
lift op (LeftF lv) _ = LeftF lv
lift op _ (LeftF lv) = LeftF lv
lift op (RightF a) (RightF b) = RightF (op a b) 

instance Prelude.Functor (EitherF left) where
    fmap _ (LeftF lv) = LeftF lv
    fmap k (RightF x) = RightF (k x)

instance Prelude.Applicative (EitherF left) where
    pure x = RightF x
    (<*>)  = lift Prelude.id

-- now lets make it a monad

eta :: a -> EitherF left a
eta  = RightF 

mu :: EitherF left (EitherF left a) -> EitherF left a
mu (LeftF lv)          = LeftF lv
mu (RightF (LeftF lv)) = LeftF lv
mu (RightF (RightF x)) = RightF x


-- lets make  EitherF a monad
-- now the kliesliLift lifting of a function
-- this is a general calculation not specific to our particular Functor
kliesliLift :: (a ->  EitherF left b) -> ( EitherF left a ->  EitherF left b)
-- The . in the next statement is function composition fromPrelude
kliesliLift k = mu . Prelude.fmap k

-- demonstrate the kliesliLift composition >==>
-- this is a general calculation not specific to our particular Functor
kliesliComposition :: (a ->  EitherF left b) -> (b ->  EitherF left c) -> ( EitherF left a ->  EitherF left c)
kliesliComposition f g = kliesliLift g . kliesliLift f

-- this is a general calculation not specific to our particular Functor
(>=>) :: (a ->  EitherF left b) -> (b ->  EitherF left c) -> ( EitherF left a ->  EitherF left c)
(>=>) = kliesliComposition

-- and from that a definition of the bind operator
-- this is a general calculation not specific to our particular Functor
bind ::  EitherF left a -> (a ->  EitherF left b) ->  EitherF left b
bind fa k = kliesliLift k fa  

-- this one is not correct
-- this is a general calculation not specific to our particular Functor
thenFunction ::  EitherF left a ->  EitherF left b ->  EitherF left b
thenFunction fa fb = bind fa (\_ -> fb) 


-- thenf Failed _ = Failed
-- thenf _ Failed = Failed 
-- thenf (Ok g) (Ok fb) = Ok fb

-- and finally make our functor a Monad
-- this is a general calculation not specific to our particular Functor
-- at least given that we have eta amd mu
instance Prelude.Monad  (EitherF left) where
    -- return x = Ok x
    return x = eta x
    (>>=) = bind
    -- (>>=) (Ok x) g = g x
    -- (>>=) Failed g = Failed
    (>>) = thenFunction
