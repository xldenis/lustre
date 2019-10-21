module Parser
  ( node
  , parse
  , errorBundlePretty
  ) where

import Data.Bifunctor

import Text.Megaparsec
import Control.Monad.Combinators.Expr

import Parser.Internal
import Name
import Syntax

node :: Parser PreNode
node = do
  _Node
  nm <- ident
  inps <- argList
  _Returns
  outs <- argList
  _Semi
  vars <- try variables <|> pure []

  _Let
  eqns <- equations
  _End
  pure $ MkNode nm inps outs vars eqns

variables = do
  _Var

  vars <- varGroup `sepEndBy1` _Semi

  pure (concat vars)
  where
  varGroup = try $ do
    vars <- ident `sepBy1` _Comma
    _Colon
    ty <- typeP

    return (map (\v -> (v, ty)) vars)

argList :: Parser [(Ident, Type)]
argList = parens (arg `sepBy1` _Comma)
  where
  arg = do
    id <- ident
    _Colon
    ty <- typeP

    pure (id, ty)

equations :: Parser [PreEquation]
equations = equation `sepEndBy1` _Semi

equation :: Parser PreEquation
equation = do
  pats <- (pure <$> try ident) <|> parens (sepBy1 ident _Comma)
  _Equals
  exp <- expression

  pure $ MkEq pats exp

expression :: Parser Expression
expression = makeExprParser primExpr
  [ [ binary _Mul (BinOp Mul)
    , binary _Div (BinOp Div)
    , binary _Mod (BinOp Mod)
    ]
  , [ binary _Add (BinOp Add)
    , binary _Sub (BinOp Sub)
    ]
  , [ binary _Le  (BinOp Le)
    , binary _Lt  (BinOp Lt)
    , binary _Gt  (BinOp Gt)
    , binary _Ge  (BinOp Ge)
    , binary _Eq  (BinOp Eq)
    , binary _Neq (BinOp Neq)
    ]
  , [ Prefix arrP
    ]
  , [ Postfix whenP
    ]
  ]
  where

  binary  name f = InfixL  (f <$ name)
  prefix  name f = Prefix  (f <$ symbol name)

  primExpr = choice
    [ Const <$> constP
    , parens expression
    , merge
    , Var <$> ident
    ]

  merge = do
    _Merge
    x <- ident

    lExp <- parens (_True >> _Arr >> expression)
    rExp <- parens (_False >> _Arr >> expression)

    pure (Merge x lExp rExp)

  arrP = try $ do
    c <- constP
    _Arr
    pure (Arr c)

  whenP = do
    _When
    c <- choice
      [ _True >> pure True
      , _False >> pure False
      ]
    x <- ident
    pure (\e -> When e c x)

constP :: Parser Const
constP = choice
  [ Int   <$> integer
  , _True  *> pure (Bool True)
  , _False *> pure (Bool False)
  , Float <$> float
  ]

typeP :: Parser Type
typeP = do
  choice
    [ _Bool  *> pure TBool
    , _Int   *> pure TInt
    , _Float *> pure TFloat
    ]
