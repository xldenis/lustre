module Name where

newtype Ident = MkI { unId :: String }
  deriving (Show, Eq)
