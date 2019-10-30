module Lust where

import           Lust.Typing
import           Lust.Machine
import           Lust.Machine.C
import           Lust.Normalization
import           Lust.Scheduling
import           Lust.Syntax
import           Lust.Error
import           Lust.Pretty

import           Control.Category          ( (>>>) )

type Pass a b an = [Node a] -> Either (Error an) [Node b]

passVerify :: [Pre Node] -> Either (Error AnsiStyle) [Clocked Node]
passVerify = runTyping

passPreCompile :: [Clocked Node] -> Either (Error an) [Clocked Node]
passPreCompile = (pure . runNormalize) >=> runScheduling

passCompileToC :: [Clocked Node] -> Either (Error an) [Doc a]
passCompileToC = runCompileToObc >>> generateC >>> pure
