-- module Main where
-- simple type class
-- 
-- Moving on, lets do another example. This time a little more extensive.
-- The topic A return type that carries accumulated error messages.
-- 
module Sample.ReturnType (ErrorReport(..), ReturnType(..), greetingsFrom) where 
import Data.Char
import Data.List

greetingsFrom :: () -> String
greetingsFrom () = "This is module ReturnType"

data ErrorReport = DivByZero String | Negative String deriving(Show)

data ReturnType a = Error ErrorReport | Good a 

lift :: (a -> b -> c) -> (ReturnType a -> ReturnType b -> ReturnType c)
lift op (Error report) _ = Error report
lift op _ (Error report) = Error report
lift op (Good a) (Good b) = Good (op a b) 

instance Functor ReturnType where
    fmap _ (Error reports) = Error reports
    fmap k (Good x)     = Good (k x)

instance Applicative ReturnType where
    pure x = Good x
    (<*>) = lift id

-- now lets make it a monad

eta :: a -> ReturnType a
eta  = Good 

mu :: ReturnType (ReturnType a) -> ReturnType a
mu (Error report) = Error report
mu (Good (Error report)) = Error report
mu (Good (Good x)) = Good x

-- lets make ReturnType a monad
-- now the kliesliLift lifting of a function
-- this is a general calculation not specific to our particular Functor
kliesliLift :: (a -> ReturnType b) -> (ReturnType a -> ReturnType b)
-- kliesliLift _ Failed = Failed
kliesliLift k = mu . fmap k

-- demonstrate the kliesliLift composition >==>
-- this is a general calculation not specific to our particular Functor
kliesliComposition :: (a -> ReturnType b) -> (b -> ReturnType c) -> (ReturnType a -> ReturnType c)
kliesliComposition f g = kliesliLift g . kliesliLift f

-- this is a general calculation not specific to our particular Functor
(>=>) :: (a -> ReturnType b) -> (b -> ReturnType c) -> (ReturnType a -> ReturnType c)
(>=>) = kliesliComposition

-- and from that a definition of the bind operator
-- this is a general calculation not specific to our particular Functor
bind :: ReturnType a -> (a -> ReturnType b) -> ReturnType b
bind fa k = kliesliLift k fa  

-- this one is not correct
-- this is a general calculation not specific to our particular Functor
thenFunction :: ReturnType a -> ReturnType b -> ReturnType b
thenFunction fa fb = bind fa (\_ -> fb) 


-- thenf Failed _ = Failed
-- thenf _ Failed = Failed 
-- thenf (Ok g) (Ok fb) = Ok fb

-- and finally make our functor a Monad
-- this is a general calculation not specific to our particular Functor
-- at least given that we have eta amd mu
instance Monad ReturnType where
    -- return x = Ok x
    return x = eta x
    (>>=) = bind
    -- (>>=) (Ok x) g = g x
    -- (>>=) Failed g = Failed
    (>>) = thenFunction
