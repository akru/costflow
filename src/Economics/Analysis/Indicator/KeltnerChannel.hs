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
import Economics.Analysis.OHLC
import Data.Vector as V

-- | Original Keltner channel estimator
keltner :: Floating a => OHLC a -> (Vector a, Vector a)
keltner ohlc = (upLine, downLine)
  where upLine   = V.zipWith (+) smaTP smaTR
        downLine = V.zipWith (-) smaTP smaTR
        smaTP    = sma 10 (typicalPrice <$> dataset ohlc)
        smaTR    = sma 10 (tradingRange <$> dataset ohlc)
        typicalPrice t = (high t + low t + close t) / 3
        tradingRange t = high t - low t
