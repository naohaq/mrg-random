{- -*- mode: haskell; coding: utf-8-unix -*-  -}
{-# LANGUAGE BangPatterns #-}

module System.Random.MRG32k3a.Internal
    (
      norm
    , m1
    , m1f
    , m2
    , m2f
    
    , mrg32k3a_rec
    , jmtxs
    ) where

import System.Random.MRG.Internal

import Data.Word (Word32, Word64)
import Data.List (unfoldr)

norm :: Double
norm = 2.328306549295728e-10
{-# INLINE norm #-}

m1 :: Word64
m1 = 4294967087
{-# INLINE m1 #-}

m1f :: Double
m1f = fromIntegral m1
{-# INLINE m1f #-}

m2 :: Word64
m2 = 4294944443
{-# INLINE m2 #-}

m2f :: Double
m2f = fromIntegral m2
{-# INLINE m2f #-}

a12 :: Word64
a12 = 1403580
{-# INLINE a12 #-}

a12f :: Double
a12f = fromIntegral a12
{-# INLINE a12f #-}

a13n :: Word64
a13n = 810728
{-# INLINE a13n #-}

a13nf :: Double
a13nf = fromIntegral a13n
{-# INLINE a13nf #-}

a21 :: Word64
a21 = 527612
{-# INLINE a21 #-}

a21f :: Double
a21f = fromIntegral a21
{-# INLINE a21f #-}

a23n :: Word64
a23n = 1370589
{-# INLINE a23n #-}

a23nf :: Double
a23nf = fromIntegral a23n
{-# INLINE a23nf #-}

floorInt :: Double -> Int
floorInt = floor

mrg32k3a_rec :: (Double,Double,Double,Double,Double,Double) -> (Double,Double)
mrg32k3a_rec (s10,s11,_  ,s20,_  ,s22) = (t1, t2)
  where p1 = a12f * s11 - a13nf * s10
        q1 = floorInt (p1 / m1f)
        r1 = p1 - fromIntegral q1 * m1f
        t1 = if r1 < 0.0 then r1 + m1f else r1
        p2 = a21f * s22 - a23nf * s20
        q2 = floorInt (p2 / m2f)
        r2 = p2 - fromIntegral q2 * m2f
        t2 = if r2 < 0.0 then r2 + m2f else r2
{-# INLINE mrg32k3a_rec #-}

mtx1 :: JumpMatrix Word64
mtx1 = JM (0, 1, 0) (0, 0, 1) (m1 - a13n, a12, 0)

mtx2 :: JumpMatrix Word64
mtx2 = JM (0, 1, 0) (0, 0, 1) (m2 - a23n, 0, a21)

cntdn :: (a -> a) -> (a, Int) -> Maybe (a, (a, Int))
cntdn _ (_, 0) = Nothing
cntdn f (x, k) = Just (y, (y, k-1))
  where y = f x

jmtxs :: [(JumpMatrix Word32,JumpMatrix Word32)]
jmtxs = zip (map (fromIntegral <$>) xs) (map (fromIntegral <$>) ys)
  where n = 64
        mtx1' = fromIntegral <$> mtx1
        xs = unfoldr (cntdn (matSqrMod m1)) (mtx1',n)
        mtx2' = fromIntegral <$> mtx2
        ys = unfoldr (cntdn (matSqrMod m2)) (mtx2',n)

-- EOF
