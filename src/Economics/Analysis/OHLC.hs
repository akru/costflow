-- |
-- Module      :  Economics.Analysis.OHLC
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- An open-high-low-close dataset (also OHLC) is a type of dataset
-- typically used to illustrate movements in the price of a financial
-- instrument over time. Each element of the set shows the price
-- range (the highest and lowest prices) over one unit of time, e.g.,
-- one day or one hour.
--
module Economics.Analysis.OHLC where

import Data.Vector (Vector)
import Data.Time (DiffTime)

-- | Price tick
data Tick a = Tick
  { open  :: !a
  , high  :: !a
  , low   :: !a
  , close :: !a
  } deriving (Eq, Show)

-- | OHLC dataset with period
data OHLC a = OHLC
  { period  :: !DiffTime
  , dataset :: !(Vector (Tick a))
  } deriving (Eq, Show)
