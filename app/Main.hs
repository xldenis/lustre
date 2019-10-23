{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Lust.Parser
import Lust.Typing
import Lust.Clocks
import Lust.Syntax
import Lust.Pretty
import Lust.Normalization
import Lust.Scheduling
import Lust.Error

import Data.Bifunctor
import Data.Function ((&))

import Control.Category ((>>>))
import Control.Monad ((>=>))

import System.Environment

compile :: [PreNode] -> Either Error' (String)
compile = runTyping
          >=> runClocking
          >=> (pure . runNormalize)
          >=> (mapM scheduleNode)
          >>> second (show . vcat . map (pretty))

main :: IO ()
main = do
  -- let x = parse (node) "" . T.pack $ unlines
  --         -- [ "node my_node (x : int, y : int) returns (z : int) ;"
  --         -- , "var work : bool;"
  --         -- , "let"
  --         -- , "z = x + y;"
  --         -- , "work = true;"
  --         -- , "y = merge work (true -> 1) (false -> 2);"
  --         -- , "end"
  --         -- ]

  --         [ "node merge_node (x : int, y : int) returns (z : int) ;"
  --         , "var c : bool;"
  --         ,     "a : int;"
  --         , "let"
  --         , "c = x % 2 == 0;"
  --         , "z = merge c (true -> x when true c) (false -> y when false c)"
  --         , "end"
  --         ]
  (f : _) <- getArgs
  x <- parseFromFile nodes f
  -- either (putStrLn . errorBundlePretty) (print . pretty) x
  either (print . prettyError) putStrLn (x & first fromParseError >>= compile)
  pure ()
