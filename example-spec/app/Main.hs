module Main where

import Language.Copilot
import Copilot.Compile.Rust

import Prelude hiding ((>), (<), div, sum, (++), (||))

temp :: Stream Word8
temp = extern "temperature" Nothing

ctemp :: Stream Float
ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0

window :: Int
window = 5

avgTemp :: Stream Float
avgTemp = sum window
  (replicate window 19.5 ++ ctemp) / fromIntegral window

spec = do
  trigger "heaton"  (ctemp < 18.0) [arg ctemp]
  trigger "heatoff" (ctemp > 21.0) [arg ctemp]
  trigger "temperature_warning" (avgTemp > 21.0 || avgTemp < 18.0) [arg avgTemp]

main = reify spec >>= compile "heater"
