{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Name where

import           Data.Hashable

newtype Ident = MkI { unId :: String }
  deriving (Show, Eq, Ord, Hashable)
