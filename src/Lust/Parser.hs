module Lust.Parser
  ( node
  , parse
  , errorBundlePretty
  , parseFromFile
  , nodes
  , fromParseError
  ) where

import           Data.Bifunctor

import           Control.Monad.Combinators.Expr
import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.Functor
import           Data.Text                          (Text)
import qualified Data.Text.IO                       as T (readFile)
import           Data.Void
import           Text.Megaparsec

import           Lust.Error
import           Lust.Name
import           Lust.Parser.Internal
import           Lust.Pretty                        (pretty, viaShow)
import           Lust.Syntax

fromParseError :: ParseErrorBundle Text Void -> Error ann
fromParseError err = Error
  { errHeader = pretty "Parse Error"
  , errKind = "parse"
  , errSummary = viaShow (errorBundlePretty err)
  , errHints = []
  }

parseFromFile :: Parser a -> FilePath -> IO (Either (ParseErrorBundle Text Void) a)
parseFromFile p file = runParser p file <$> T.readFile file

nodes :: Parser [PreNode]
nodes = some node

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
  pats <- (pure <$> try ident) <|> parens (NE.sepBy1 ident _Comma)
  _Equals
  MkEq () pats <$> expression

expression :: Parser Expression
expression = makeExprParser primExpr
  [ [ binary _Mul (BinOp Mul)
    , binary _Div (BinOp Div)
    , binary _Mod (BinOp Mod)
    ]
  , [ binary _Add (BinOp Add)
    , binary (try _Sub) (BinOp Sub)
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
    , app
    , Var <$> ident
    ]

  app = try $ do
    f <- ident
    args <- parens (expression `sepBy1` _Comma)
    _Every
    App f args <$> ident
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
  , _True  $> Bool True
  , _False $> Bool False
  , Float <$> float
  ]

typeP :: Parser Type
typeP =
  choice
  [ _Bool Data.Functor.$> TBool
  , _Int Data.Functor.$> TInt
  , _Float Data.Functor.$> TFloat
  ]
