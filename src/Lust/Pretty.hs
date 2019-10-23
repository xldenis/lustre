module Lust.Pretty
( module Data.Text.Prettyprint.Doc
, module Lust.Pretty
, module Data.Text.Prettyprint.Doc.Render.Terminal
) where

import           Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

above :: Doc a -> Doc a -> Doc a
above a b = vsep [a, b]

dot' :: Doc a
dot' = pretty '•'

bulleted :: [Doc a] -> Doc a
bulleted = vcat . map (dot' <+>)
