{- -*- mode: haskell; coding: utf-8-unix -*- -}
{-# LANGUAGE BangPatterns             #-}

module System.Random.MRG63k3a.Internal
    (
      norm
    , m1
    , m2

    , mrg63k3a_rec
    , jmtxs
    ) where

import System.Random.MRG.Internal

import Data.Int  (Int64)
import Data.Word (Word64)
import Data.List (unfoldr)

norm :: Double
norm = 1.0842021724855052e-19
{-# INLINE norm #-}

m1 :: Int64
m1 = 9223372036854769163
{-# INLINE m1 #-}

m2 :: Int64
m2 = 9223372036854754679
{-# INLINE m2 #-}

-- a12 * q12 + r12 = m1  ... (1)
a12 :: Int64
a12 = 1754669720
{-# INLINE a12 #-}

q12 :: Int64
q12 = 5256471877
{-# INLINE q12 #-}

r12 :: Int64
r12 = 251304723
{-# INLINE r12 #-}

-- a13n * q13 + r13 = m1 ... (2)
-- a13n * q13 = m1 - r13 ... (2)'
a13n :: Int64
a13n = 3182104042
{-# INLINE a13n #-}

q13 :: Int64
q13 = 2898513661
{-# INLINE q13 #-}

r13 :: Int64
r13 = 394451401
{-# INLINE r13 #-}

-- a21 * q21 + r21 = m2 ... (3)
a21 :: Int64
a21 = 31387477935
{-# INLINE a21 #-}

q21 :: Int64
q21 = 293855150
{-# INLINE q21 #-}

r21 :: Int64
r21 = 143639429
{-# INLINE r21 #-}

-- a23n * q23 + r23 = m2 ... (4)
a23n :: Int64
a23n = 6199136374
{-# INLINE a23n #-}

q23 :: Int64
q23 = 1487847900
{-# INLINE q23 #-}

r23 :: Int64
r23 = 985240079
{-# INLINE r23 #-}

mrg63k3a_rec :: (Int64,Int64,Int64,Int64,Int64,Int64) -> (Int64,Int64)
mrg63k3a_rec (s10,s11,_ ,s20,_ ,s22)
  = (t1,t2)
  where (h10,j10) = s10 `divMod` q13
        --  h10 * q13 + j10 = s10
        --  a13n * j10 - h10 * r13
        --    = a13n * (s10 - h10 * q13) - h10 * r13
        --    = a13n * s10 - h10 * (a13n * q13 + r13)
        --    { because of a13n * q13 + r13 = m1 (2), }
        --    = a13n * s10 - h10 * m1
        --
        p13  = a13n * j10 - h10 * r13
        -- When s10 = k * q13  (k = 1,2,..),
        --   a13n * s10 = a13n * k * q13
        --   { because of a13n * q13 = m1 - r13 (2)', }
        --      = k * (m1 - r13)
        --      = k * m1 - k * r13
        --      = (k-1) * m1 + (m1 - k * r13),
        -- while
        --   (h10, j10) = (k,0)  { because of s10 = k * q13 }
        --   a13n * j10 - h10 * r13 = - k * r13.
        -- Therefore,
        --   a13n * j10 - h10 * r13 + m1 = m1 - k * r13
        --      = a13n * s10 - (k-1) * m1
        --
        p13' = if p13 < 0 then p13 + m1 else p13
        (h11,j11) = s11 `divMod` q12
        --  h11 * q12 + j11 = s11
        --  a12 * j11 - h11 * r12
        --    = a12 * (s11 - h11 * q12) - h11 * r12
        --    = a12 * s11 - h11 * (a12 * q12 + r12)
        --    = a12 * s11 - h11 * m1
        --
        p12  = a12  * j11 - h11 * r12
        p12' = if p12  < 0 then p12  + m1 - p13' else p12 - p13'
        !t1  = if p12' < 0 then p12' + m1 else p12'
        (h20,j20) = s20 `divMod` q23
        --  h20 * q23 + j20 = s20
        --  a23n * j20 - h20 * r23
        --    = a23n * (s20 - h20 * q23) - h20 * r23
        --    = a23n * s20 - h20 * (a23n * q23 + r23)
        --    = a23n * s20 - h20 * m2
        --
        p23  = a23n * j20 - h20 * r23
        p23' = if p23 < 0 then p23 + m2 else p23
        (h22,j22) = s22 `divMod` q21
        --  h22 * q21 + j22 = s22
        --  a21 * j22 - h22 * r21
        --    = a21 * (s22 - h22 * q21) - h22 * r21
        --    = a21 * s22 - h22 * (a21 * q21 + r21)
        --    = a21 * s22 - h22 * m2
        --
        p21  = a21  * j22 - h22 * r21
        p21' = if p21  < 0 then p21  + m2 - p23' else p21 - p23'
        !t2  = if p21' < 0 then p21' + m2 else p21'
{-# INLINE mrg63k3a_rec #-}

mtx1 :: JumpMatrix Int64
mtx1 = JM (0, 1, 0) (0, 0, 1) (m1 - a13n, a12, 0)

mtx2 :: JumpMatrix Int64
mtx2 = JM (0, 1, 0) (0, 0, 1) (m2 - a23n, 0, a21)

cntdn :: (a -> a) -> (a, Int) -> Maybe (a, (a, Int))
cntdn _ (_, 0) = Nothing
cntdn f (x, k) = Just (y, (y, k-1))
  where y = f x

jmtxs :: [(JumpMatrix Word64, JumpMatrix Word64)]
jmtxs = zip xs ys
  where n = 64
        m1' = fromIntegral m1 :: Word64
        m2' = fromIntegral m2 :: Word64
        mtx1' = fromIntegral <$> mtx1
        xs = mtx1' : unfoldr (cntdn (matSqrModW64 m1')) (mtx1',n)
        mtx2' = fromIntegral <$> mtx2
        ys = mtx2' : unfoldr (cntdn (matSqrModW64 m2')) (mtx2',n)

-- EOF
