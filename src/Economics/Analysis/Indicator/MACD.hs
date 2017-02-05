-- |
-- Module      :  Economics.Analysis.Indicator.MACD
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- The MACD indicator (or "oscillator") is a collection of three
-- time series calculated from historical price data, most often
-- the closing price. These three series are: the MACD series proper,
-- the "signal" or "average" series, and the "divergence" series which
-- is the difference between the two. The MACD series is the difference
-- between a "fast" (short period) exponential moving average (EMA), and
-- a "slow" (longer period) EMA of the price series. The average series is
-- an EMA of the MACD series itself.
--
module Economics.Analysis.Indicator.MACD where

import Economics.Analysis.Indicator.MA
import Data.Vector.Unboxed as V

-- | The MACD indicator
macd' :: (Floating a, Unbox a) => Int -> Int -> Int
                               -> Vector a -> (Vector a, Vector a, Vector a)
macd' a b c dataset = (series, signal, divergence)
  where series = V.zipWith (-) (ema a dataset) (ema b dataset)
        signal = ema c series
        divergence = V.zipWith (-) series signal

-- | Default (12, 26, 9) MACD indicator
macd :: (Floating a, Unbox a) => Vector a -> (Vector a, Vector a, Vector a)
{-# INLINE macd #-}
macd = macd' 12 26 9
