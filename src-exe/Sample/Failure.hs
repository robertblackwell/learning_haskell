module Sample.Failure (greetingsFrom, Failure (..)) where
-- simple type class
import Data.Char
import Data.List

greetingsFrom :: () -> String
greetingsFrom () = "This is module ReturnType"


-- for some monad details see
-- https://wiki.haskell.org/Monads_as_computation#Do_notation
-- and also
-- https://wiki.haskell.org/All_About_Monads#Introduction_2
-- https://wiki.haskell.org/All_About_Monads
-- https://www.uibk.ac.at/mathematik/algebra/staff/fritz-tobias/ct2021_course_projects/category_theory.pdf


data Failure a = Failed | Ok a deriving (Show, Eq)

addInt (a,b) = a + b

divInt :: Int -> Int -> Failure Int
divInt _ 0 = Failed
divInt a b = Ok $ a `div` b

-- ------------------------------------------------------------
-- make the type class into a Functor
-- ------------------------------------------------------------

instance Functor Failure  where
    fmap afunction (Ok an_a) = Ok $ afunction an_a
    fmap afunction Failed = Failed

tf :: Int -> Failure Int
tf 0 = Failed
tf b = Ok (7 `div` b)

ff :: Int -> Int
ff x = x + 3

-- this is a functor lifting of morphisms
liftedff :: Failure Int -> Failure Int
liftedff = fmap ff

--
-- The next bit is the real significance of Applicative
-- 


-- ------------------------------------------------------------
-- now make it an Applicative functor
-- ------------------------------------------------------------
-- this is the key feature of an Applicatove functor - it lifts Currying
mlift :: (a -> b -> c) -> (Failure a -> Failure b -> Failure c)
mlift op Failed _ = Failed
mlift op _ Failed = Failed
mlift op (Ok a) (Ok b) = Ok (op a b)
-- we now have some machinery that can make our functor an Applicative
instance Applicative Failure where 
    pure x = Ok x
    -- (<*>) Failed _ = Failed
    -- (<*>) _ Failed = Failed
    -- (<*>) (Ok g) (Ok x) = Ok $ g x
    (<*>) = mlift id    

-- ------------------------------------------------------------
-- Turning to Monads - first we give the mathematical definition of
-- a monad for Failure
-- ------------------------------------------------------------

eta:: a -> Failure a
eta = Ok 

mu :: Failure (Failure a) -> Failure a
mu Failed      = Failed
mu (Ok Failed) = Failed
mu (Ok (Ok x)) = Ok x 

-- now the Kliesli lifting of a function
-- this is a general calculation not specific to our particular Functor
kliesli :: (a -> Failure b) -> (Failure a -> Failure b)
-- kliesli _ Failed = Failed
kliesli k = mu . fmap k

-- demonstrate the Kliesli composition >==>
-- this is a general calculation not specific to our particular Functor
kliesliComposition :: (a -> Failure b) -> (b -> Failure c) -> (Failure a -> Failure c)
kliesliComposition f g = kliesli g . kliesli f

-- this is a general calculation not specific to our particular Functor
(>=>) :: (a -> Failure b) -> (b -> Failure c) -> (Failure a -> Failure c)
(>=>) = kliesliComposition

-- and from that a definition of the bind operator
-- this is a general calculation not specific to our particular Functor
bind :: Failure a -> (a -> Failure b) -> Failure b
bind fa k = kliesli k fa  

-- this one is not correct
-- this is a general calculation not specific to our particular Functor
thenf :: Failure a -> Failure b -> Failure b
thenf fa fb = bind fa (\_ -> fb) 


-- thenf Failed _ = Failed
-- thenf _ Failed = Failed 
-- thenf (Ok g) (Ok fb) = Ok fb

-- and finally make our functor a Monad
-- this is a general calculation not specific to our particular Functor
-- at least given that we have eta amd mu
instance Monad Failure where
    -- return x = Ok x
    return x = eta x
    (>>=) = bind
    -- (>>=) (Ok x) g = g x
    -- (>>=) Failed g = Failed
    (>>) = thenf
