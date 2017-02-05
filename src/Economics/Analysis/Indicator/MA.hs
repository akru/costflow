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

import Data.Vector as V

-- | A simple moving average (SMA) is an arithmetic moving average
-- calculated by adding the closing price of the security for
-- a number of time periods and then dividing this total by the
-- number of time periods.
sma :: Floating a => Int -> Vector a -> Vector a
sma n dataset = average . window <$> [0 .. V.length dataset - n]
  where average  = (/ fromIntegral n) . V.sum
        window t = V.slice t (t + n) dataset

-- | An exponential moving average (EMA) is a type of moving average
-- that is similar to a simple moving average (SMA), except that
-- more weight is given to the latest data. It's also known as the
-- exponentially weighted moving average. This type of moving average
-- reacts faster to recent price changes than a simple moving average.
ema :: Floating a => Int -> Vector a -> Vector a
ema n dataset = V.scanl ema' (V.head dataset) (V.tail dataset)
  where ema' e p = alpha * p + (1 - alpha) * e
        alpha    = 2 / (fromIntegral n + 1)
