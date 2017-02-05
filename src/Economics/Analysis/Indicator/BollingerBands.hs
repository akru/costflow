{-# LANGUAGE OverloadedLists #-}
-- |
-- Module      :  Economics.Analysis.Indicator.BollingerBands
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Bollinger Bands is a tool invented by John Bollinger in the 1980s.
-- Having evolved from the concept of trading bands, Bollinger Bands
-- can be used to measure the "highness" or "lowness" of the price
-- relative to previous trades. Bollinger Bands are a volatility
-- indicator similar to the Keltner channel.
--
module Economics.Analysis.Indicator.BollingerBands (bollinger) where

import Economics.Analysis.Indicator.MA
import Economics.Analysis.OHLC
import Data.Vector as V

bollinger :: Floating a => OHLC a -> (Vector a, Vector a)
bollinger ohlc = (upLine, downLine)
  where upLine   = V.zipWith (+) ma sigma
        downLine = V.zipWith (-) ma sigma
        ma       = sma 20 closed
        sigma    = (* 2) <$> stddev 20 closed
        closed   = close <$> dataset ohlc

stddev :: Floating a => Int -> Vector a -> Vector a
stddev n v = stddev' <$> [0 .. V.length v - n]
  where window t  = V.slice t (t + n) v
        delta t   = (^ (2 :: Int)) . (sma n v ! t -) <$> window t
        stddev' t = sqrt (V.sum (delta t) / fromIntegral n)
