-- module Main where
-- simple type class
-- 
-- Moving on, lets do another example. This time a little more extensive.
-- The topic A return type that carries accumulated error messages.
-- 
{-# LANGUAGE DataKinds, KindSignatures #-}
module Sample.State (State(..), greetingsFrom) where 
import Prelude hiding (Either, Left, Right)
import Data.Char
import Data.List


greetingsFrom :: () -> Prelude.String
greetingsFrom () = "This is module State"

-- Either is a bit like our  Trace monad except that the "Error" piece is allowed to be any type (with some restrictions)
-- However like the  Trace the two sides are not equal in the way they area treasted


newtype State s a  = State{runState :: s -> (s, a)} 

instance Show (State s a) where
    show state = "<<instancce State >>"

-- 
-- The state monad is a function equivalent of the following OO pattern.
-- We have an object as follows (in Typescript)
--
--  class Obj{
--      private prop: string
--      constructor(p: string) {this.prop = p}
--      updateProp(): string {
--          p = this.prop
--          this.prop = p + p
--          return p
--      } 
-- }
-- 
-- which can be converted to a functional patterm as follows (again in TYpescript)
-- 
-- type StateVariable = {prop: string}
-- function updateProp(statevariable: StateVariable): [StateVariable, string] {
--      const p = stateVariable.prop
--      const newStateVariable = {prop : p + p}
--      return [newStateVariable, p]
-- }
-- 
-- the state monad is observed in this example as
-- type State = {runState: updatePropr}
-- 
-- or more generally
-- 
-- type State<A> = {runState : <A>(sv:StateVariable) => [StateVariable, A]}
-- 
-- or even more generally
-- 
-- type State<S, A> = {runState : <S, A>(sv: S) => [S, A]}
-- 
-- 

myRunstate :: a -> s -> (s, a)
myRunstate x y = (y, x)

myStateinstance = State{runState = myRunstate "Hello world myStateinstance"}

mkState :: (s -> (s, a)) -> State s a
mkState f = State{runState = f}


-- apply a state instances runState function to a value of type s
apply :: State s a -> s -> (s, a)
-- I would prefer this to be
-- apply stateInstance sValue = runState stateInstance $ sValue
-- but the style guide complains resulting in less understandable code
apply = runState

-- utility functions that will be handy below
-- apply a state instances runState function and then collect either the
-- first or second component
applyGetFirst :: State s a -> s -> s
applyGetFirst stateInstance sVal = fst $ apply stateInstance sVal

applyGetSecond :: State s a -> s -> a
applyGetSecond stateInstance sVal = snd $ apply stateInstance sVal

-- this is fmap at the level of the runState values inside a State instance
-- a lot of mental gymnastics to simply apply f in the second slot of a pair
-- could have used the Pair monad to do this
-- This is a preliminary to defining fmap for the State functor
liftHelper :: (a -> b) -> (s -> (s, a)) -> (s -> (s, b))
liftHelper f aRunStateInstance sValue = (sValue, f $ snd $ aRunStateInstance sValue ) 

f1 :: Int-> Int
f1 x = x + 11

f2 :: String -> (String, String)
f2 x = (x, "{{ " ++ x ++ "}}")

sf1 :: String -> (String, Int)
sf1 astring = (astring, 99)

zz = liftHelper f1 sf1

ww = liftHelper f2 $ runState myStateinstance 

vv = mkState ww


-- fMap :: (a -> b) -> (State s a -> State s b)
-- fMap f aStateInstance = 
-- now for a given `left` Trace left is going to be a monad

instance Prelude.Functor (State s) where
    fmap f astate = mkState $ liftHelper f $ runState astate
 
-- now lets make the State functor an Applicative

-- again we start by building a helper function that creates the s->(s,c) function
-- we need to create the finals State instance
liftA2Helper :: (a -> b -> c) -> (State s a -> State s b -> (s -> (s,c)))
liftA2Helper fab2c aStateValue bStateValue sValue =  (sValue, cV)
    where
        (_, aV) = runState aStateValue sValue
        (_, bV) = runState bStateValue sValue
        cV      = fab2c aV bV
        -- the next 3 lines are a less idiomatic alternative
        -- aValue = applyGetSecond aStateValue sValue
        -- bValue = applyGetSecond bStateValue sValue
        -- cValue = fab2c aValue bValue

-- I just realized that ListA2Hlper is preserving binary operators
-- we also need to preserve nullary operators
-- and then we have preservation of n-ary operators
liftA0Helper :: a -> (s -> (s,  a))
liftA0Helper aValue = \sV -> (sV, aValue)
    -- the style checker wants me to change this to
    -- liftA0Helper aValue = (, aValue)
    -- but warns that I might nned to add {- LANGUAGE TupleSections  -} to the top of the file
    -- I see no need to do this. The recommended notation adds no value

liftA2 :: (a -> b -> c) -> (State s a -> State s b -> State s c)
liftA2 fab2c aStateValue bStateValue = State{runState = f} 
    where
        f = liftA2Helper fab2c aStateValue bStateValue

liftA0 :: a -> State s a
liftA0 aV = mkState $ liftA0Helper aV

instance Prelude.Applicative (State s) where
    -- there must be some way to derive fure from 
    pure x = liftA0 x 
    -- the style checker wants me to change this to
    -- pure x = mkState $ (, x)
    -- but warns that I might nned to add {- LANGUAGE TupleSections  -} to the top of the file
    -- I see no need to do this. The recommended notation adds no value
    (<*>)  = liftA2 Prelude.id


-- now lets make it a monad

eta :: a -> State s a
eta  = liftA0

muHelper :: State s (State s a) -> (s -> (s,  a))
muHelper state2Instance sValue = result
    where 
        (sV1, v1) = runState state2Instance sValue  
        (sV2, v2) = runState v1 sV1
        result = (sV2, v2)

mu :: State s (State s a) -> State s a
mu state2Instance = mkState $ \s -> muHelper state2Instance s

-- now the kliesliLift lifting of a function
-- this is a general calculation not specific to our particular Functor
kliesliLift :: (a ->  State s b) -> (State s a ->  State s b)
-- The . in the next statement is function composition fromPrelude
kliesliLift k = mu . Prelude.fmap k

-- demonstrate the kliesliLift composition >==>
-- this is a general calculation not specific to our particular Functor
kliesliComposition :: (a -> State s b) -> (b ->  State s c) -> (State s a ->  State s c)
kliesliComposition f g = kliesliLift g . kliesliLift f

-- this is a general calculation not specific to our particular Functor
(>=>) :: (a ->  State s b) -> (b ->  State s c) -> (State s a ->  State s c)
(>=>) = kliesliComposition

-- and from that a definition of the bind operator
-- this is a general calculation not specific to our particular Functor
bind :: State s a -> (a ->  State s b) ->  State s b
bind fa k = kliesliLift k fa  

-- this one is not correct
-- this is a general calculation not specific to our particular Functor
thenFunction :: State s a -> State s b -> State s b
thenFunction fa fb = bind fa (\_ -> fb) 
-- ignore the style suggestion


-- and finally make our functor a Monad
-- this is a general calculation not specific to our particular Functor
-- at least given that we have eta amd mu
instance Prelude.Monad  (State a) where
    -- return x = Ok x
    return x = eta x
    (>>=) = bind
    -- (>>=) (Ok x) g = g x
    -- (>>=) Failed g = Failed
    (>>) = thenFunction
