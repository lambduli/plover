module Lexer.Lexer where

import Control.Monad.State ( MonadState(get, put), gets, StateT( runStateT ), State )
import Control.Monad.Except ( Except, runExcept )

import Data.Word ( Word8 )
import Data.Char ( ord )
import Data.List ( uncons )

import Token ( Token )
import Token qualified


token :: Token -> Lexer Token
token t = return t


emit :: (String -> Token) -> String -> Lexer Token
emit mk't str = return (mk't str)


get'line'no :: Lexer Int
get'line'no = gets (ai'line'no . lexer'input)


get'col'no :: Lexer Int
get'col'no = gets (ai'col'no . lexer'input)


eval'parser :: Lexer a -> String -> Either (String, Int) (a, Lexer'State)
eval'parser parser source = runExcept $! runStateT parser (initial'state source)


type Lexer a = StateT Lexer'State (Except (String, Int)) a


data AlexInput = Input
  { ai'line'no   :: !Int
  , ai'col'no    :: !Int
  , ai'last'char :: !Char
  , ai'input     :: String }
  deriving (Eq, Show)


data Lexer'State = Lexer'State
  { lexer'input :: !AlexInput }
  deriving (Eq, Show)


initial'state :: String -> Lexer'State
initial'state s = Lexer'State
  { lexer'input       = Input
                        { ai'line'no    = 1
                        , ai'col'no     = 1 
                        , ai'last'char  = '\n'
                        , ai'input      = s } }
