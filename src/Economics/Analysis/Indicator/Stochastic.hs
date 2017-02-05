{-# LANGUAGE OverloadedLists #-}
-- |
-- Module      :  Economics.Analysis.Indicator.Stochastic
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- The stochastic oscillator is a momentum indicator that
-- uses support and resistance levels. The term stochastic
-- refers to the point of a current price in relation to its
-- price range over a period of time. This method attempts to
-- predict price turning points by comparing the closing price
-- of a security to its price range.
--
module Economics.Analysis.Indicator.Stochastic where

import Economics.Analysis.Indicator.MA
import Economics.Analysis.OHLC
import Data.Vector.Unboxed as V

stochastic :: (Ord a, Unbox a, Floating a) => OHLC a -> (Vector a, Vector a)
{-# INLINE stochastic #-}
stochastic = stochastic' 5 3

stochastic' :: (Ord a, Unbox a, Floating a)
            => Int -> Int -> OHLC a -> (Vector a, Vector a)
stochastic' k n ohlc = (signal, sma n signal)
  where signal' v = (close (V.last v) - minLow v) / (maxHigh v - minLow v) * 100
        signal   = V.map (signal' . window) [0 .. V.length (dataset ohlc) - k]
        minLow   = V.minimum . V.map low
        maxHigh  = V.minimum . V.map high
        window t = V.slice t k (dataset ohlc)
