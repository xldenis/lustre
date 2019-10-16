{
module Lexer (runLexer, WithPos(..), LexToken(..), OpTok(..)) where

import Text.Megaparsec.Pos
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ ;
  "#".*   ;
  $digit+ { tok1 (LInt . read) }
  $digit+ \. $digit+ { tok1 (LFloat . read) }

  "node"    { tok Node }
  "returns" { tok Returns }
  "var"     { tok LVar }
  "let"     { tok Let }
  "end"     { tok End }
  "then"  { tok Then  }
  "else"  { tok Else  }
  "if"    { tok If    }
  "true"  { tok LTrue }
  "false" { tok LFalse }

  "("     { tok (LParen) }
  ")"     { tok (RParen) }
  ":"     { tok (Colon ) }
  ";"     { tok (Semi  ) }
  ","     { tok (Comma ) }
  "+"     { tok (Op LAdd) }
  "-"     { tok (Op LSub) }
  "*"     { tok (Op LMul) }
  "/"     { tok (Op LDiv) }
  "%"     { tok (Op LMod) }
  "->"    { tok (Op LArr) }
  "=="    { tok (Op LEq ) }
  "!="    { tok (Op LNeq) }
  "<"     { tok (Op LLt ) }
  ">"     { tok (Op LGt ) }
  "<="    { tok (Op LLe ) }
  ">="    { tok (Op LGe ) }
  "!"     { tok (Op LNot) }
  "&&"    { tok (Op LAnd) }
  "||"    { tok (Op LOr ) }

  "="     { tok Equal    }
  $alpha [$alpha $digit \_]* { tok1 Ident }

{

data OpTok
  = LEq | LNeq | LLt | LLe | LGt | LGe
  | LAdd | LSub | LMul | LDiv | LMod
  | LNot
  | LAnd | LOr | LArr
  deriving (Eq, Show, Ord)

data LexToken
  = Node
  | LInt Int
  | LBool Bool
  | LFloat Float
  | LParen
  | RParen
  | Op OpTok
  | Ident String
  | LVar
  | Semi
  | Returns
  | End
  | Comma
  | If
  | Then
  | Else
  | Colon
  | EOF
  | Equal
  | Let
  | LTrue
  | LFalse
  deriving (Eq, Show, Ord)

data WithPos a = WithPos
  { startPos :: SourcePos
--  , endPos   :: SourcePos
  , tokenVal :: a
  } deriving (Eq, Ord, Show)

aPosLine (AlexPn _ l _) = mkPos l
aPosCol  (AlexPn _ _ c) = mkPos c

fromAlexPos :: String -> AlexPosn -> SourcePos
fromAlexPos fname aPos = SourcePos fname (aPosLine aPos) (aPosCol aPos)

located :: AlexAction a -> AlexAction (WithPos a)
located lex i@(pos, _, _, _) l = do
  f <- getFile
  WithPos (fromAlexPos f pos) <$> lex i l

tok :: a -> AlexAction (WithPos a)
tok  t = located go
  where go _ _ = return t

tok1 :: (String -> a) -> AlexAction (WithPos a)
tok1 f  = located go
  where go (_, _, _, s) len = return (f (take len s))

alexEOF :: Alex (WithPos LexToken)
alexEOF = return (WithPos undefined EOF)

type AlexUserState = String
alexInitUserState = "<no file>"

getFile = Alex $ \s -> Right (s, alex_ust s)

runLexer :: String -> String -> Either String [WithPos LexToken]
runLexer file str = runAlex str (setFile >> go)
  where
  setFile = Alex $ \s -> Right (s{alex_ust = file}, ())

  go = do
    tok <- alexMonadScan
    case tokenVal tok of
      EOF -> return []
      t   -> go >>= \l -> return (tok : l)


}
