{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Parser

main :: IO ()
main = do
  let x = parse node "" . T.pack $ unlines
          [ "node my_node (x : int, y : int) returns (z : int) ;"
          , "var work : bool;"
          , "let"
          , "z = x + y;"
          , "a = if true then 1 else 2;"
          , "end"
          ]

  case x of
    Right e -> print e
    Left e -> putStrLn $ errorBundlePretty e

