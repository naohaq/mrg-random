{- -*- mode: haskell; coding: utf-8-unix -*-  -}
{-# LANGUAGE BangPatterns #-}

module System.Random.MRG.Internal
  (
    StateVector(..)
  , JumpMatrix(..)

  , vecTrOn
  , matMulOn
  , matSqrOn
  ) where

data StateVector a = SV (a,a,a) deriving (Show,Eq)
data JumpMatrix a = JM (a,a,a) (a,a,a) (a,a,a) deriving (Show,Eq)

instance Functor StateVector where
  fmap f (SV (x1,x2,x3)) = SV (f x1, f x2, f x3)

instance Functor JumpMatrix where
  fmap f (JM (x11,x12,x13) (x21,x22,x23) (x31,x32,x33))
    = JM (f x11,f x12,f x13) (f x21,f x22,f x23) (f x31,f x32,f x33)

mulOn :: (Integral a) => a -> a -> a -> a
mulOn m x y = (x * y) `mod` m
{-# INLINE mulOn #-}

dotOn :: (Integral a) => a -> (a,a,a) -> StateVector a -> a
dotOn m (x1,x2,x3) (SV (y1,y2,y3)) = z
  where u = mulOn m
        !z = ((x1 `u` y1) + (x2 `u` y2) + (x3 `u` y3)) `mod` m
{-# INLINE dotOn #-}

vecTrOn :: (Integral a) => a -> JumpMatrix a -> StateVector a -> StateVector a
vecTrOn m (JM xr1 xr2 xr3) y = SV (z1,z2,z3)
  where u = dotOn m
        !z1 = xr1 `u` y
        !z2 = xr2 `u` y
        !z3 = xr3 `u` y
{-# INLINE vecTrOn #-}

matMulOn :: (Integral a) => a -> JumpMatrix a -> JumpMatrix a -> JumpMatrix a
matMulOn m xx (JM (y11,y12,y13) (y21,y22,y23) (y31,y32,y33))
  = JM (z11,z12,z13) (z21,z22,z23) (z31,z32,z33)
  where u = vecTrOn m
        SV (!z11,!z21,!z31) = xx `u` (SV (y11,y21,y31))
        SV (!z12,!z22,!z32) = xx `u` (SV (y12,y22,y32))
        SV (!z13,!z23,!z33) = xx `u` (SV (y13,y23,y33))

matSqrOn :: (Integral a) => a -> JumpMatrix a -> JumpMatrix a
matSqrOn m xx = matMulOn m xx xx

-- EOF
