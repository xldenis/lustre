{-# LANGUAGE TypeFamilies, RecordWildCards #-}
module Parser.Internal
  ( module Parser.Internal
  , LexToken(..)
  , OpTok(..)
  , runLexer
  ) where

import Control.Monad (void)

import Data.Proxy
import Data.Void
import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set

import Text.Megaparsec

import Lexer
import Name

newtype TokStream = MkStream
  { unStream :: [WithPos LexToken]
  } deriving (Show, Eq)

instance Stream TokStream where
  type Token TokStream = WithPos LexToken
  type Tokens TokStream = [WithPos LexToken]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null

  take1_ (MkStream []) = Nothing
  take1_ (MkStream (t:ts)) = Just (t, MkStream ts)

  takeN_ n (MkStream s)
    | n <= 0    = Just ([], MkStream s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in Just (x, MkStream s')
  takeWhile_ f (MkStream s) =
    let (x, s') = DL.span f s
    in (x, MkStream s')
  showTokens Proxy = DL.intercalate ", "
    . NE.toList
    . fmap (show . tokenVal)
  reachOffset o pst@PosState {..} =
    case drop (o - pstateOffset) (unStream pstateInput) of
      [] ->
        ( pstateSourcePos
        , "<missing input>"
        , pst { pstateInput = MkStream [] }
        )
      (x:xs) ->
        ( startPos x
        , "<missing input>"
        , pst { pstateInput = MkStream (x:xs) }
        )

-- showMyToken :: MyToken -> String
-- showMyToken = \case
--   (Int n)    -> show n
--   Plus       -> "+"
--   Mul        -> "*"
--   Div        -> "/"
--   OpenParen  -> "("
--   CloseParen -> ")"

type Parser = Parsec Void TokStream

liftTok :: LexToken -> WithPos LexToken
liftTok tok = WithPos pos tok
  where
    pos = initialPos ""

tok :: LexToken -> Parser LexToken
tok c = token test (Set.singleton . Tokens . nes . liftTok $ c)
  where
    test wpos@(WithPos _ x) =
      if x == c
        then Just x
        else Nothing
    nes x = x :| []

op :: OpTok -> Parser ()
op = void . tok . Op

integer :: Parser Int
integer = token test Set.empty <?> "integer"
  where
    test (WithPos _ (LInt n)) = Just n
    test _ = Nothing

float :: Parser Float
float = token test Set.empty <?> "float"
  where
    test (WithPos _ (LFloat n)) = Just n
    test _ = Nothing


ident :: Parser Ident
ident = token test Set.empty <?> "identifier"
  where
    test (WithPos _ (Ident n)) = Just $ MkI n
    test _ = Nothing

string :: String -> Parser ()
string s = token test (Set.singleton . Tokens . nes . liftTok $ Ident s) <?> s
  where
    test (WithPos _ (Ident n)) | s == n = Just ()
    test _ = Nothing

    nes x = x :| []
