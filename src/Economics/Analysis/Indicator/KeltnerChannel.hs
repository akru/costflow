-- |
-- Module      :  Economics.Analysis.Indicator.KeltnerChannel
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Keltner channel is a technical analysis indicator showing a central
-- moving average line plus channel lines at a distance above and below.
--
module Economics.Analysis.Indicator.KeltnerChannel where

import Economics.Analysis.Indicator.MA
import Economics.Analysis.OHLC (OHLC(..))
import Data.Vector.Unboxed as V

-- | Original Keltner channel estimator
keltner :: (Unbox a, Floating a) => OHLC a -> (Vector a, Vector a)
keltner ohlc = (upLine, downLine)
  where upLine   = V.zipWith (+) smaTP smaTR
        downLine = V.zipWith (-) smaTP smaTR
        smaTP    = sma 10 (V.map typicalPrice $ dataset ohlc)
        smaTR    = sma 10 (V.map tradingRange $ dataset ohlc)
        typicalPrice (_, high, low, close) = (high + low + close) / 3
        tradingRange (_, high, low, _)     = high - low
