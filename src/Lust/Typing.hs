module Lust.Typing where

import           Lust.Typing.Clocks       as C
import           Lust.Typing.Types        as T

import           Control.Monad             ( (>=>) )

runTyping = T.runTyping >=> C.runClocking
