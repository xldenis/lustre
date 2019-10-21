module Pretty
( module Data.Text.Prettyprint.Doc
, module Pretty
) where

import Data.Text.Prettyprint.Doc

above :: Doc a -> Doc a -> Doc a
above a b = vsep [a, b]
