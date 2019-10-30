
module Main where

import qualified Data.Text                as T

import           Lust.Error
import           Lust.Machine
import           Lust.Normalization
import           Lust.Parser
import           Lust.Pretty
import           Lust.Scheduling
import           Lust.Syntax
import           Lust.Machine.C
import           Lust

import           Data.Bifunctor
import           Data.Function             ( (&) )

import           Control.Category          ( (>>>) )
import           Control.Monad             ( (>=>) )

import           System.Environment

compile :: [PreNode] -> Either Error' String
compile = passVerify >=> passPreCompile >=> passCompileToC >>> second (show . vcat)

main :: IO ()
main = do
  (f : _) <- getArgs
  x       <- parseFromFile nodes f
  either (print . prettyError) putStrLn (x & first fromParseError >>= compile)
  pure ()
