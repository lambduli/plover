module Main where

import Data.List ( foldl', intercalate, isSuffixOf )
import Data.List.Extra ( trim )

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import System.IO ( hFlush, stdout, openFile, IOMode(ReadMode), hGetContents )


import Term ( Term(..), Struct(..), Predicate(..), Goal(..) )

import Evaluate.Step ( step )
import Evaluate.State ( State(..), Action(..), Qu(..), Direction(..) )

import Parser.Natural qualified ( parse'base, parse'query )
import Parser.Prolog qualified ( parse'base, parse'query )


empty'state :: State
empty'state = State { base = []
                    , path'q = Empty
                    , counter = 0 }


set'goal :: [Goal] -> State -> State
set'goal goals state = state{ path'q = qu
                            , counter = 0 }
  where
    free'names :: [String]
    free'names = Set.toList (free'vars'in'query goals)

    free'vars = map Var free'names

    q'vars = Map.fromList $! zip free'names free'vars

    qu = Qu{ before = [], current = (goals, q'vars), after = [], direction = Forward }


load'base :: [Predicate] -> State -> Bool -> State
load'base base state is'natural = state { base = base
                                        , path'q = Empty
                                        , counter = 0
                                        , is'natural = is'natural }


main :: IO ()
main = do
  putStrLn "Plover — a toy automated theorem prover."
  repl empty'state
  putStrLn "Bye!"


repl :: State -> IO ()
repl old'state = do
  putStr "?- "
  hFlush stdout
  str <- getLine
  case str of
    ":q" -> return ()
    ":Q" -> return ()
    ':' : 'l' : 'o' : 'a' : 'd' : file'path -> do
      file'handle <- openFile (trim file'path) ReadMode
      file'content <- hGetContents file'handle
      let is'natural = ".nad" `isSuffixOf` file'path
      let parse = if is'natural then Parser.Natural.parse'base else Parser.Prolog.parse'base
      case parse file'content of
        Left (err, col) -> do
          let padding = take (3 + col - 1) $! repeat ' '
          putStrLn $! padding ++ "^"
          putStrLn err
          repl old'state
        Right new'base -> do
          let new'state = load'base new'base old'state is'natural
          repl new'state

    ':' : _ -> do
      putStrLn "I don't know this command, sorry."
      repl old'state

    _ -> do
      let is'nat = is'natural old'state
      let parse = if is'nat then Parser.Natural.parse'query else Parser.Prolog.parse'query
      case parse str of
        Left (err, col) -> do
          let padding = take (3 + col - 1) $! repeat ' '
          putStrLn $! padding ++ "^"
          putStrLn err
          repl old'state
        Right goals -> do
          let new'state = set'goal goals old'state
          try'to'prove new'state


try'to'prove :: State -> IO ()
try'to'prove state = do
  case step state of
    Succeeded s -> do
      case step s of
        Redoing state' -> do
          let Qu{ current = (_, q'vars) } = path'q s
          let result =  if Map.null q'vars
                        then "True"
                        else intercalate "\n" $! map (\ (k, v) -> k ++ " = " ++ show v) $! Map.toList q'vars
          putStrLn result
          user'input <- getLine
          case user'input of
            ":next" -> do
              putStrLn "  or\n"
              try'to'prove state'
            ":done" -> do
              putStrLn "."
              repl state'
            _ -> do
              putStrLn "  or\n"
              try'to'prove state'

        Done -> do
          let Qu{ current = (_, q'vars) } = path'q s
          let result =  if Map.null q'vars
                        then "True"
                        else intercalate "\n" $! map (\ (k, v) -> k ++ " = " ++ show v) $! Map.toList q'vars
          putStrLn result
          repl s

        _ -> error "should never happen"

      -- TODO: wait for the interaction
      -- to know whether to attempt backtracking.
      -- I should change the step, so that the first two equations are not there
      -- another function would do that for me.
      -- that would allow me to sort of re-charge the state
      -- without misleadingly calling `step` or `try'to'prove`
      {-  Maybe it is not misleading. Maybe keeping the step's pattern
          matching exhaustive is worth it.  -}
      -- that function would either set me up for backtracking
      -- that would be signalized by `Redoing`
      -- or it would recognize that there is no way to backtrack
      -- so that would be signalized by `Done`.
      -- This would have the nice property of me knowing
      -- right away, whether I should hang for users's interaction
      -- or if I should just put `.` right away.

    Failed -> do
      putStrLn "False."
      repl state

    Searching s -> do
      try'to'prove s

    _ -> error "should never happen"


free'vars'in'query :: [Goal] -> Set.Set String
free'vars'in'query goals = foldl' (\ set g -> set `Set.union` free'vars'in'goal g) Set.empty goals


free'vars'in'goal :: Goal -> Set.Set String
free'vars'in'goal (Call fun) = free'vars'in'functor fun
free'vars'in'goal (Unify val'l val'r) = Set.union (free'vars'in'val val'l) (free'vars'in'val val'r)


free'vars'in'functor :: Struct -> Set.Set String
free'vars'in'functor Struct{ args } = foldl' (\ set g -> set `Set.union` free'vars'in'val g) Set.empty args


free'vars'in'val :: Term -> Set.Set String
free'vars'in'val (Var name) = Set.singleton name
free'vars'in'val (Atom _) = Set.empty
free'vars'in'val (Compound fun) = free'vars'in'functor fun
free'vars'in'val Wildcard = Set.empty
