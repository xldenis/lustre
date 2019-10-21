{-# LANGUAGE OverloadedStrings #-}
module Parser.Internal
  ( module Parser.Internal
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Name

import Control.Monad (void)
import Data.Void
import Data.Text (Text)

type Parser = Parsec Void Text

scn :: Parser ()
scn = L.space (void spaceChar) (L.skipLineComment "#") empty

sc :: Parser ()
sc = L.space (void $ oneOf [' ', '\t']) (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol scn

reserved :: [String]
reserved = ["if", "then", "else", "end", "node", "returns", "merge", "when"]

ident :: Parser Ident
ident = p >>= res
  where
  p = label "identifier" . lexeme $ ((:) <$> lowerChar <*> many identLetters)
  res i = if i `elem` reserved then
      fail $ "The reserved word `" ++ i ++ "` cannot be used as an identifier."
    else
      return (MkI i)

  identLetters = oneOf letters

  letters :: String
  letters = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

float :: Parser Float
float = lexeme L.float

integer :: Parser Int
integer = lexeme L.decimal

_Returns = symbol "returns"
_End    = symbol "end"
_Node   = symbol "node"
_Let    = symbol "let"
_Var    = symbol "var"
_Equals = symbol "="
_Semi   = symbol ";"
_Comma  = symbol ","
_Colon  = symbol ":"

_False = symbol "false"
_True  = symbol "true"
_Arr   = symbol "->"

_Merge = symbol "merge"
_When  = symbol "when"

_Bool = symbol "bool"
_Int  = symbol "int"
_Float = symbol "float"

{- Operators -}
_Neq = symbol "!="
_Eq  = symbol "=="
_Ge  = symbol ">="
_Gt  = symbol ">"
_Le  = symbol "<="
_Lt  = symbol "<"

_And = symbol "&&"
_Or = symbol "||"

_Add = symbol "+"
_Sub = symbol "-"
_Mul = symbol "*"
_Div = symbol "/"
_Mod = symbol "%"
