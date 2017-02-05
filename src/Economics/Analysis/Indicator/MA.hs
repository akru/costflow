{-# LANGUAGE OverloadedLists #-}
-- |
-- Module      :  Economics.Analysis.Indicator.MA
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Moving average based indicators
--
module Economics.Analysis.Indicator.MA where

import Data.Vector.Unboxed as V

-- | A simple moving average (SMA) is an arithmetic moving average
-- calculated by adding the closing price of the security for
-- a number of time periods and then dividing this total by the
-- number of time periods.
sma :: (Unbox a, Floating a) => Int -> Vector a -> Vector a
sma n dataset = V.map (average . window) [0 .. V.length dataset - n]
  where average  = (/ fromIntegral n) . V.sum
        window t = V.slice t n dataset

-- | An exponential moving average (EMA) is a type of moving average
-- that is similar to a simple moving average (SMA), except that
-- more weight is given to the latest data. It's also known as the
-- exponentially weighted moving average. This type of moving average
-- reacts faster to recent price changes than a simple moving average.
ema :: (Unbox a, Floating a) => Int -> Vector a -> Vector a
ema n dataset = V.scanl ema' (V.head dataset) (V.tail dataset)
  where ema' e p = alpha * p + (1 - alpha) * e
        alpha    = 2 / (fromIntegral n + 1)

-- | Kaufman's Adaptive Moving Average (KAMA) is a moving average
-- designed to account for market noise or volatility. KAMA will
-- closely follow prices when the price swings are relatively small
-- and the noise is low. KAMA will adjust when the price swings widen
-- and follow prices from a greater distance. This trend-following
-- indicator can be used to identify the overall trend, time turning
-- points and filter price movements.
ama :: (Unbox a, Floating a) => Vector a -> Vector a
ama dataset = V.scanl ama' (dataset ! n) [n + 1 .. V.length dataset - 1]
  where signal t  = abs (dataset ! t - dataset ! (t - n - 1))
        delta t i = abs (dataset ! (t - i) - dataset ! (t - i - 1))
        noise t   = V.sum (V.map (delta t) [0 .. n - 1])
        er t      = signal t / noise t
        sc t      = (er t * (fastest - slowest) + slowest) ^ (2 :: Int)
        n         = 10
        fastest   = 2 / (2 + 1)
        slowest   = 2 / (30 + 1)
        ama' a t  = sc t * (dataset ! t) + (1 - sc t) * a
