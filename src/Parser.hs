module Parser where

import Data.Bifunctor

import Text.Megaparsec
import Control.Monad.Combinators.Expr

import Parser.Internal
import Name
import Syntax

parse' :: Parser a -> String -> Either String a
parse' parser str =
  runLexer "" str >>= first errorBundlePretty . parse (parser <* eof) "" . MkStream

node :: Parser Node
node = do
  tok Node
  nm <- ident
  inps <- argList
  tok Returns
  outs <- argList
  tok Semi
  vars <- variables

  tok Let
  eqns <- equations
  tok End
  pure $ MkNode nm inps outs vars eqns

variables = do
  tok LVar

  vars <- varGroup `sepEndBy1` tok Semi

  pure (concat vars)
  where
  varGroup = do
    vars <- ident `sepBy1` tok Comma
    tok Colon
    ty <- typeP

    return (map (\v -> (v, ty)) vars)

parens :: Parser a -> Parser a
parens = between (tok LParen) (tok RParen)

argList :: Parser [(Ident, Type)]
argList = parens (arg `sepBy1` (tok Comma))
  where
  arg = do
    id <- ident
    tok Colon
    ty <- typeP

    pure (id, ty)

equations :: Parser [Equation]
equations = equation `sepEndBy1` tok Semi

equation :: Parser Equation
equation = do
  pats <- (pure <$> ident) <|> parens (sepBy1 ident (tok Comma))
  tok Equal
  exp <- expression

  pure $ MkEq pats exp

expression :: Parser Expression
expression = makeExprParser primExpr
  [ [ binary LMul (BinOp Mul)
    , binary LMul (BinOp Div)
    , binary LMod (BinOp Mod)
    ]
  , [ binary LAdd (BinOp Add)
    , binary LSub (BinOp Sub)
    ]
  , [ binary LLe (BinOp Le)
    , binary LLt (BinOp Lt)
    , binary LGt (BinOp Gt)
    , binary LGe (BinOp Ge)
    , binary LEq (BinOp Eq)
    , binary LNeq (BinOp Neq)
    ]
  , [ Prefix arrP
    ]
  ]
  where

  primExpr = choice
    [ Const <$> constP
    , parens expression
    , Var <$> ident
    , ifte
    ]

  ifte = do
    tok If
    c <- expression
    tok Then
    a <- expression
    tok Else
    b <- expression

    pure (Ifte c a b)

  arrP = try $ do
    c <- constP
    op LArr
    pure (Arr c)

  binary  name f = InfixL  (f <$ op name)
  prefix  name f = Prefix  (f <$ op name)

constP :: Parser Const
constP = choice
  [ Int <$> integer
  , tok LTrue *> pure (Bool True)
  , tok LFalse *> pure (Bool False)
  , Float <$> float
  ]

typeP :: Parser Type
typeP = do
  choice
    [ string "bool" *> pure TBool
    , string "int"  *> pure TInt
    , string "float" *> pure TFloat
    ]
