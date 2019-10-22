{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Parser
import Typing
import Clocks

import Syntax

import Pretty

import Normalization
import Scheduling

import Data.Bifunctor
import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Function ((&))
import System.Environment

compile :: [PreNode] -> Either String String
compile = runTyping  >>> first show
          >=> runClocking >>> first show
          >=> (pure . runNormalize)
          >=> (mapM scheduleNode) >>> first show
          >>> second (show . pretty . map (fmap eraseClocks))

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
  either putStrLn putStrLn (x & first errorBundlePretty >>= compile)
  pure ()
