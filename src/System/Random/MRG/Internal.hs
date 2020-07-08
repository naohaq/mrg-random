{- -*- mode: haskell; coding: utf-8-unix -*-  -}
{-# LANGUAGE BangPatterns #-}

module System.Random.MRG.Internal
  (
    StateVector(..)
  , JumpMatrix(..)

  , vecTrMod
  , matMulMod
  , matSqrMod

  , vecTrModW64
  , matMulModW64
  , matSqrModW64
  ) where

import Data.Word (Word64)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))

data StateVector a = SV (a,a,a) deriving (Show,Eq)
data JumpMatrix a = JM (a,a,a) (a,a,a) (a,a,a) deriving (Show,Eq)

instance Functor StateVector where
  fmap f (SV (x1,x2,x3)) = SV (f x1, f x2, f x3)

instance Functor JumpMatrix where
  fmap f (JM (x11,x12,x13) (x21,x22,x23) (x31,x32,x33))
    = JM (f x11,f x12,f x13) (f x21,f x22,f x23) (f x31,f x32,f x33)

hiW64 :: Word64 -> Word64
hiW64 x = (x `shiftR` 32) .&. 0x00000000ffffffff
{-# INLINE hiW64 #-}

loW64 :: Word64 -> Word64
loW64 x = x .&. 0x00000000ffffffff
{-# INLINE loW64 #-}

splitW64 :: Word64 -> (Word64,Word64)
splitW64 x = (hiW64 x, loW64 x)
{-# INLINE splitW64 #-}

mulW64 :: Word64 -> Word64 -> (Word64,Word64)
mulW64 x y = (vh,vl)
  where (xh,xl) = splitW64 x
        (yh,yl) = splitW64 y
        z0 = xl * yl
        z1 = xh * yl
        z2 = xl * yh
        z3 = xh * yh
        (z0h,z0l) = splitW64 z0
        (z1h,z1l) = splitW64 z1
        (z2h,z2l) = splitW64 z2
        w0 = z0h + z1l + z2l
        w1 = loW64 w0 `shiftL` 32
        w2 = hiW64 w0
        !vl = w1 .|. z0l
        !vh = z3 + z1h + z2h + w2
{-# INLINE mulW64 #-}

mulModW64 :: Word64 -> Word64 -> Word64 -> Word64
mulModW64 m x y = go $ x `mulW64` y
  where s  = (18446744073709551615 `mod` m) + 1
        s' = if s >= m then s - m else s
        go v = case step v of
                 (0, t0l) -> t0l `mod` m
                 t1 -> go t1
        step (wh,wl) = (vh, vlh .|. vll)
          where rl        = wl `mod` m
                rh        = wh `mod` m
                (rlh,rll) = splitW64 rl
                -- b  = p *m + s
                -- ah = qh*m + rh
                -- al = ql*m + rl
                -- ah*b + al
                --    = (qh*m + rh)*(p*m + s) + ql*m + rl
                --    = (qh*m*p + qh*s + rh*p + ql)*m + rh*s + rl
                (zh ,zl ) = rh `mulW64` s'
                (zlh,zll) = splitW64 zl
                t0        = rll + zll
                (c0 ,vll) = splitW64 t0
                t1        = rlh + zlh + c0
                vlh       = loW64 t1 `shiftL` 32
                c1        = hiW64 t1
                vh        = zh + c1
{-# INLINE mulModW64 #-}

dotModW64 :: Word64 -> (Word64,Word64,Word64) -> StateVector Word64 -> Word64
dotModW64 m (x1,x2,x3) (SV (y1,y2,y3)) = z
  where u = mulModW64 m
        !w = ((x1 `u` y1) + (x2 `u` y2)) `mod` m
        !z = (w + (x3 `u` y3)) `mod` m
{-# INLINE dotModW64 #-}

vecTrModW64 :: Word64 -> JumpMatrix Word64 -> StateVector Word64 -> StateVector Word64
vecTrModW64 m (JM xr1 xr2 xr3) y = SV (z1,z2,z3)
  where u = dotModW64 m
        !z1 = xr1 `u` y
        !z2 = xr2 `u` y
        !z3 = xr3 `u` y
{-# INLINE vecTrModW64 #-}

matMulModW64 :: Word64 -> JumpMatrix Word64 -> JumpMatrix Word64 -> JumpMatrix Word64
matMulModW64 m xx (JM (y11,y12,y13) (y21,y22,y23) (y31,y32,y33))
  = JM (z11,z12,z13) (z21,z22,z23) (z31,z32,z33)
  where u = vecTrModW64 m
        SV (!z11,!z21,!z31) = xx `u` (SV (y11,y21,y31))
        SV (!z12,!z22,!z32) = xx `u` (SV (y12,y22,y32))
        SV (!z13,!z23,!z33) = xx `u` (SV (y13,y23,y33))
{-# INLINE matMulModW64 #-}

matSqrModW64 :: Word64 -> JumpMatrix Word64 -> JumpMatrix Word64
matSqrModW64 m xx = matMulModW64 m xx xx
{-# INLINE matSqrModW64 #-}

mulMod :: (Integral a) => a -> a -> a -> a
mulMod m x y = (x * y) `mod` m
{-# INLINE mulMod #-}

dotMod :: (Integral a) => a -> (a,a,a) -> StateVector a -> a
dotMod m (x1,x2,x3) (SV (y1,y2,y3)) = z
  where u = mulMod m
        !z = ((x1 `u` y1) + (x2 `u` y2) + (x3 `u` y3)) `mod` m
{-# INLINE dotMod #-}

vecTrMod :: (Integral a) => a -> JumpMatrix a -> StateVector a -> StateVector a
vecTrMod m (JM xr1 xr2 xr3) y = SV (z1,z2,z3)
  where u = dotMod m
        !z1 = xr1 `u` y
        !z2 = xr2 `u` y
        !z3 = xr3 `u` y
{-# INLINE vecTrMod #-}

matMulMod :: (Integral a) => a -> JumpMatrix a -> JumpMatrix a -> JumpMatrix a
matMulMod m xx (JM (y11,y12,y13) (y21,y22,y23) (y31,y32,y33))
  = JM (z11,z12,z13) (z21,z22,z23) (z31,z32,z33)
  where u = vecTrMod m
        SV (!z11,!z21,!z31) = xx `u` (SV (y11,y21,y31))
        SV (!z12,!z22,!z32) = xx `u` (SV (y12,y22,y32))
        SV (!z13,!z23,!z33) = xx `u` (SV (y13,y23,y33))

matSqrMod :: (Integral a) => a -> JumpMatrix a -> JumpMatrix a
matSqrMod m xx = matMulMod m xx xx

-- EOF
