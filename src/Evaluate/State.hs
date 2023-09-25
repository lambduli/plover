module Evaluate.State where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Term ( Goal, Term, Predicate )


{-  The Action data structure is there
    to signalize what happened in the last step -}
data Action a = Succeeded !a
              | Failed
              | Searching !a
              | Redoing !a
              | Done
  deriving (Eq, Show)


type Path = ([Goal], Map.Map String Term)


data State
  = State { base :: ![Predicate]
          -- , query'vars :: !(Map.Map String Term)

          , path'q :: Qu Path

          , counter :: !Int }
  deriving (Eq, Show)


-- TODO: A new state representation that encodes:
-- Processing a current goal'stack
-- Succeeded - does not contain the goal'stack (I think)
-- Failed - does not contain the goal'stack (might be interesting to think about how to represent what failed and why)
-- 
-- The main idea is that there is no Redoing and Done
-- and also the goal'stack is NonEmpty
-- this eliminates the need foor those two equations in `step`
-- because this book keeping will be done in a different function
-- a function that takes a state like Succeeded, one that does not contain a goal'stack
-- and either populates the goal'stack for Processing/Searching or decides that it is Done.
-- This seems like more sensible approach.


data Qu a = Qu{ before    :: [a]
              , current   :: a
              , after     :: [a]
              , direction :: Direction }
          | Empty
  deriving (Eq, Show)


data Direction = Forward | Backward
  deriving (Eq, Show)
