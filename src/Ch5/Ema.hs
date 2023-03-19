module Ch5.Ema where

import qualified Control.Foldl as F

data Interpolation = Previous | Linear | Next

data TsValue = TsValue {timestamp :: Double, value :: Double} deriving Show

data EMA = EMA {time :: Double, val :: Double, prevVal :: Double} deriving Show

ema :: Interpolation -> Double -> F.Fold TsValue EMA
ema interpolation τ = F.Fold step (EMA 0 0 0) id
  where
    step (EMA t1 m1 p1) (TsValue t0 x0) = EMA t0 (μ * m1 + (ν - μ) * x0 + (1 - ν) * p1) x0
      where
        α = (t1 - t0) / τ
        μ = exp (- α)
        ν = case interpolation of
          Previous -> 1
          Linear -> (1 - μ) / α
          Next -> μ
