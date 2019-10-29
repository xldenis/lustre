
module Main where

import qualified Data.Text          as T

import           Lust.Clocks
import           Lust.Error
import           Lust.Machine
import           Lust.Normalization
import           Lust.Parser
import           Lust.Pretty
import           Lust.Scheduling
import           Lust.Syntax
import           Lust.Typing
-- import Lust.Machine.C

import           Data.Bifunctor
import           Data.Function      ((&))

import           Control.Category   ((>>>))
import           Control.Monad      ((>=>))

import           System.Environment

compile :: [PreNode] -> Either Error' String
compile = runTyping
          >=> runClocking
          >=> (pure . runNormalize)
          >=> mapM scheduleNode
          >=> (pure . map nodeToObc)
          -- >=> (pure . map generateC)
          -- >>> second (show . vcat)
          >>> second (show . vcat . map pretty)

main :: IO ()
main = do
  (f : _) <- getArgs
  x <- parseFromFile nodes f
  either (print . prettyError) putStrLn (x & first fromParseError >>= compile)
  pure ()
