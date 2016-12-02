-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CpuFreq
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu frequency monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.CpuFreq where

import Plugins.Monitors.Common
import Plugins.Monitors.CoreCommon

-- |
-- Cpu frequency default configuration. Default template contains only
-- one core frequency, user should specify custom template in order to
-- get more cpu frequencies.
cpuFreqConfig :: IO MConfig
cpuFreqConfig =
  mkMConfig "Freq: <cpu0>" (map ((++) "cpu" . show) [0 :: Int ..])


-- |
-- Function retrieves monitor string holding the cpu frequency (or
-- frequencies)
runCpuFreq :: [String] -> Monitor String
runCpuFreq _ = do
  suffix <- getConfigValue useSuffix
  decimals <- getConfigValue decDigits
  let path = ["/sys/devices/system/cpu/cpu", "/cpufreq/scaling_cur_freq"]
      divisor = 1e6 :: Double
      trunc :: (Integral n) => n -> Double -> Double
      trunc n = (/10.0^^n) . fromInteger . round . (*10^n) 
      fmt = if suffix then old_format else show . trunc decimals 
      old_format x 
            | x < 1 = show (round (x * 1000) :: Integer) ++ "MHz"
            | otherwise = show x ++ "GHz" 
  failureMessage <- getConfigValue naString
  checkedDataRetrieval failureMessage [path] Nothing (/divisor) fmt
