module Evaluate.Step where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List ( mapAccumL )


import Evaluate.State ( State(..), Action(..), Qu(..), Path, Direction(..) )

import Term ( Predicate(..), Struct(..), Term(..), Goal(..) )


step :: State -> Action State
-- In these two equations we handle the situation
-- when in the previous step we have successfully proved the whole goal
-- that leaves us with an empty goal'stack.
-- These two equations handle the situation when no backtracking can happen (empty backtracking'stack)
-- or if some backtracking can happen (Redoing).
-- It's not ideal as the idea that the Action transitions from Succeeded to either Done or Redoing is only in our heads.
-- It would be much better if it could be encoded in the design so that the type system and pattern matching
-- exhaustivity checker would be able to check this.
step state@State{ path'q = Empty }
  = Done

{-  This awkward pattern must be checked too.
    The current goal can be done and it can also be the last goal
    and this is what that'd look like.  -}
step state@State{ path'q = Qu { before = []
                              , current = ([], _)
                              , after = [] } }
  = Done

{-  NOTE: In this case, at least one (before, or after) has to be non-empty.  -}
step state@State{ path'q = q@Qu { current = ([], _)
                                , after
                                , direction = Forward } }
  = case after of
      p : ps -> Redoing state { path'q = q{ current = p
                                          , after = ps } }
      {-  If the `after` list is empty, we assume that the `before` list is not empty.
          We just change the direction and let the recursion do the work. -}
      [] -> step state{ path'q = q{ direction = Backward } }

step state@State{ path'q = q@Qu { before
                                , current = ([], _)
                                , direction = Backward } }
  = case before of
      p : ps -> Redoing state { path'q = q{ before = ps
                                          , current = p } }
      {-  If the `before` list is empty, we assume that the `after` list is not empty.
          We just change the direction and let the recursion do the work. -}
      [] -> step state{ path'q = q{ direction = Forward }}

{-  The above should deal with the situations when the previous step finished one path.
    It could mean finishing one of many paths left or finishing the last one.
    Both those situations should be handled.  -}

{-  TODO: Work on this one. -}
{-  PROVE CALL  -}
step state@State{ base
                , path'q = q@Qu{ current = (Call f@Struct{ name, args } : goals, q'vars) }
                , counter }
  = let predicates = look'for f base
        (new'counter, paths) = mapAccumL to'path counter predicates
        path'qs = add'paths paths q
    in  case pop'path path'qs of
          Nothing -> -- should not really happen, a few of them were just added
            error "should never happen"
          Just new'path'q ->
            Searching state { path'q = new'path'q
                            , counter = new'counter }

  where look'for :: Struct -> [Predicate] -> [Predicate]
        look'for _ [] = []
        
        look'for f@Struct{ name, args } (fact@(Fact (Struct{ name = name', args = args' })) : base)
          | name == name' && length args == length args' = fact : look'for f base
          | otherwise = look'for f base
        
        look'for f@Struct{ name, args } (rule@(Struct{ name = name', args = args' } :- body) : base)
          | name == name' && length args == length args' = rule : look'for f base
          | otherwise = look'for f base


        -- uses `f` and `q'vars` and `goals` from the surrounding scope
        to'path :: Int -> Predicate -> (Int, ([Goal], Map.Map String Term))
        to'path counter (Fact (Struct{ args = patterns }))
          = let (counter', patterns') = rename'all patterns counter
                goals' = map (uncurry Unify) (zip args patterns')
                new'goal'stack = goals' ++ goals

            in  (counter', (new'goal'stack, q'vars))


        to'path counter (Struct{ args = patterns } :- body)
          = let (counter', patterns', body') = rename'both patterns body counter
                head'goals = map (uncurry Unify) (zip args patterns')
                new'goal'stack = head'goals ++ body' ++ goals

            in  (counter', (new'goal'stack, q'vars))


{-  PROVE UNIFICATION -}
step state@State{ path'q = q@Qu { current = (Unify value'l value'r : goal'stack, query'vars) } }
  = case unify (value'l, value'r) goal'stack query'vars of
      Nothing ->
        -- could not unify
        -- this means that this goal, fails
        fail'and'backtrack state
      Just (new'goal'stack, new'query'vars) ->
        -- they can be unified and the new'environment reflects that
        -- just return a new state with stack and env changed
        succeed state { path'q = q{ current = (new'goal'stack, new'query'vars) } }


{-  This function is called when the unifiction-step succeeds.
    This might mean a complete success for the current path or it might mean that there are yet more goals to deal with.  -}
succeed :: State -> Action State
{-  If the current path is done, we report success. One of the first equations of `step` will deal with backtracking. -}
succeed state@State{ path'q = Qu{ current = ([], _) } }
  = Succeeded state

{-  If the current path is the only path, we just keep searching it.  -}
succeed state@State{ path'q = Qu{ before = []
                                , after = [] } }
  = Searching state

{-  If the direction goes `Forward` but `after` is empty (and `before` is not), we just switch the direction and try again. -}
succeed state@State{ path'q = q@Qu{ direction = Forward
                                  , after = [] } }
  = succeed state{ path'q = q{ direction = Backward } }

{-  If the direction goes `Forward` and `after` is non-empty, we put the `current` on `before` and set the `current` to the next path.  -}
succeed state@State{ path'q = q@Qu{ before
                                  , current
                                  , direction = Forward
                                  , after = path : paths } }
  = Searching state{ path'q = q { before = current : before
                                , current = path
                                , after = paths } }

succeed state@State{ path'q = q@Qu{ direction = Backward
                                  , before = [] }}
  = succeed state{ path'q = q{ direction = Forward }}

succeed state@State{ path'q = q@Qu{ before = path : paths
                                  , current
                                  , direction = Backward
                                  , after } }
  = Searching state{ path'q = q { before = paths
                                , current = path
                                , after = current : after }}


-- The following function fails the current goal.
-- It needs to replace the current goal'stack with a top of the backtracking one.
-- That means re-setting the position and the environment.
-- The counter stays the same (because it only increments).
{-  TODO:
          So I need to drop the 'current' goal-path and depending on the direction,
          set the 'current' to a new goal-path.
          I must make sure that there is no chance of starvation.
          If the direction is towards a non-empty part of the queue,
          we must make sure to select the next one from that part.
          We only flip the direction if the corresponding part of the queue is empty.          
 -}
fail'and'backtrack :: State -> Action State
fail'and'backtrack State{ path'q = Qu { before = []
                                      , after = [] } }
  = Failed

fail'and'backtrack state@State{ path'q = q@Qu { direction = Forward
                                              , after = [] } }
  = fail'and'backtrack state{ path'q = q{ direction = Backward } }

fail'and'backtrack state@State{ path'q = q@Qu { direction = Forward
                                              , after = path : paths } }
  = step state{ path'q = q{ current = path
                          , after = paths } }

fail'and'backtrack state@State{ path'q = q@Qu { direction = Backward
                                              , before = [] } }
  = fail'and'backtrack state{ path'q = q{ direction = Forward } }

fail'and'backtrack state@State{ path'q = q@Qu { direction = Backward
                                              , before = path : paths } }
  = step state{ path'q = q{ current = path
                          , before = paths } }


rename'all :: [Term] -> Int -> (Int, [Term])
rename'all patterns counter = (counter', patterns')
  where
    ((counter', mapping), patterns') = mapAccumL rename'val (counter, Map.empty) patterns


rename'val :: (Int, Map.Map String String) -> Term -> ((Int, Map.Map String String), Term)
rename'val (cntr, mapping) (Var name)
  = if Map.member name mapping
    then ((cntr, mapping), Var (mapping Map.! name))
    else  let new'name = "_" ++ show cntr
              new'cntr = cntr + 1
              new'mapping = Map.insert name new'name mapping
          in  ((new'cntr, new'mapping), Var new'name)

rename'val state (Compound (Struct{ name, args }))
  = let (state', args') = mapAccumL rename'val state args
    in  (state', Compound (Struct{ name = name, args = args' }))

rename'val acc val
  = (acc, val)


rename'both :: [Term] -> [Goal] -> Int -> (Int, [Term], [Goal])
rename'both patterns goals counter = (counter', patterns', goals')
  where
    (state, patterns') = mapAccumL rename'val (counter, Map.empty) patterns

    (state', goals') = mapAccumL rename'goal state goals

    (counter', _) = state'


rename'goal :: (Int, Map.Map String String) -> Goal -> ((Int, Map.Map String String), Goal)
rename'goal state (Call (Struct{ name, args }))
  = let (state', args') = mapAccumL rename'val state args
    in  (state', Call (Struct{ name, args = args' }))
rename'goal state (Unify val'l val'r)
  = let (state', [val'l', val'r']) = mapAccumL rename'val state [val'l, val'r]
    in  (state', Unify val'l' val'r')


unify :: (Term, Term) -> [Goal] -> Map.Map String Term -> Maybe ([Goal], Map.Map String Term)
{-  DELETE  (basically) -}
unify (Wildcard, _) goals query'vars = Just (goals, query'vars)
unify (_, Wildcard) goals query'vars = Just (goals, query'vars)

{-  DELETE  -}
unify (Atom a, Atom b) goals query'vars
  | a == b = Just (goals, query'vars)
  | otherwise = Nothing

{-  DECOMPOSE + CONFLICT  -}
unify ( Compound Struct{ name = name'a, args = args'a }
      , Compound Struct{ name = name'b, args = args'b })
      goals query'vars
  | name'a /= name'b || length args'a /= length args'b = Nothing  -- CONFLICT
  | otherwise = Just (arg'goals ++ goals, query'vars)             -- DECOMPOSE
  where
    arg'goals :: [Goal]
    arg'goals = zipWith Unify args'a args'b

{-  ELIMINATE + OCCURS  -}
unify (Var a, value) goals query'vars
  | (Var a) == value = Just (goals, query'vars) -- DELETE (both are variables)
  | occurs a value = Nothing                    -- OCCURS CHECK (the one on the right is not a variable so I can do the check!)
  | otherwise = Just (substituted'goals, substituted'query'vars)
  where
    substituted'goals = map (subst'goal (a, value)) goals
    substituted'query'vars = Map.map (subst'val (a, value)) query'vars

    subst'goal :: (String, Term) -> Goal -> Goal
    subst'goal substitution (Call fun) = Call substituted'fun
      where substituted'fun = subst'functor substitution fun
    subst'goal substitution (Unify val'a val'b) = Unify substituted'val'a substituted'val'b
      where substituted'val'a = subst'val substitution val'a
            substituted'val'b = subst'val substitution val'b

    subst'val :: (String, Term) -> Term -> Term
    subst'val (from, to) (Var name)
      | name == from = to
      | otherwise = Var name
    subst'val _ (Atom name) = Atom name
    subst'val substitution (Compound fun) = Compound (subst'functor substitution fun)
    subst'val _ Wildcard = Wildcard

    subst'functor :: (String, Term) -> Struct -> Struct
    subst'functor substitution Struct{ name, args } = Struct{ name, args = substituted'args }
      where substituted'args = map (subst'val substitution) args

{-  SWAP  (because of the above equation, we assume the `value` not being a variable) -}
unify (value, Var b) goals query'vars = unify (Var b, value) goals query'vars

unify _ _ _ = Nothing   -- CONFLICT (for atoms and structs)


occurs :: String -> Term -> Bool
occurs var'name (Var name) = var'name == name
occurs var'name (Atom _) = False
occurs var'name (Compound Struct{ args }) = any (occurs var'name) args
occurs var'name Wildcard = False


add'paths :: [Path] -> Qu Path -> Qu Path
add'paths paths q@Qu{ before, direction = Forward } = q{ before = paths ++ before }
add'paths paths q@Qu{ after, direction = Backward } = q{ after = paths ++ after }


pop'path :: Qu Path -> Maybe (Qu Path)
pop'path Qu{ before = [], after = [] }
  = Nothing
--  at the end, changing direction
pop'path Qu{ before = (p : ps), after = [], direction = Forward }
  = Just Qu{ before = ps, current = p, after = [], direction = Backward }
--  -->
pop'path Qu{ before, after = p : ps, direction = Forward }
  = Just Qu{ before, current = p, after = ps, direction = Forward }
--  at the beginning again, changing direction
pop'path Qu{ before = [], after = p : ps, direction = Backward }
  = Just Qu{ before = [], current = p, after = ps, direction = Forward }
--  <--
pop'path Qu{ before = p : ps, after, direction = Backward }
  = Just Qu{ before = ps, current = p, after, direction = Backward }
