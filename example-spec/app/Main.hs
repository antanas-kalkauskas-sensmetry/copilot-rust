-- This example implements a simple home heating system. The system heats
-- when the temperature gets too low, and stops when it is high enough. It read
-- temperature as a byte (range -50C to 100C) and translates this to Celsius.

module Main where

import Language.Copilot
import qualified Language.Copilot as Copilot
import Copilot.Compile.Rust

import Prelude hiding ((>), (<), div)

-- External temperature as a byte, ranging from -50C to 100C.
temp :: Stream Word8
temp = extern "temperature" Nothing

-- Temperature in Celsius.
--
-- We need to cast the Word8 to a Float. This is an unsafeCast, as there
-- is no direct relation between Word8 and Float.
ctemp :: Stream Float
ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0

window :: Int
window = 5

avgTemp :: Stream Float
avgTemp = Copilot.sum window
  (replicate window 19.5 Copilot.++ ctemp) / fromIntegral window

spec = do
  -- Triggers that fire when the ctemp is too low or too high,
  -- pass the current ctemp as an argument.
  trigger "heaton"  (ctemp < 18.0) [arg ctemp]
  trigger "heatoff" (ctemp > 21.0) [arg ctemp]
  trigger "temperature_warning" (avgTemp > 21.0 Copilot.|| avgTemp < 18.0) [arg avgTemp]

-- Compile the spec
main = reify spec >>= compile "heater"