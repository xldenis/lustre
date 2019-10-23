module Lust.Error
  ( module Lust.Error
  , module Control.Monad.Except
  ) where

import           Control.Monad.Except
import           Lust.Pretty

type Error' = Error AnsiStyle

data Error a = Error
  { errHeader  :: Doc a -- name of error (subject)
  , errKind    :: String -- subsystem (sender)
  , errSummary :: Doc a -- details of error (body)
  , errHints   :: [Doc a] -- solutions
  }

prettyError :: Error AnsiStyle -> Doc AnsiStyle
prettyError err = nest 2 $ (annotate bold $ errHeader err)
    `above` (errSummary err)
    `above` bulleted (map (\d -> align $ hint<+> d) (errHints err))
  where
  hint = annotate (color Magenta <> bold) (pretty "hint:")

rethrow :: MonadError e m => (e -> e) -> m a -> m a
rethrow f action = action `catchError` (throwError . f )

