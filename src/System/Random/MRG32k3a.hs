{- -*- mode: haskell; coding: utf-8-unix -*-  -}
{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, FlexibleContexts,
    ScopedTypeVariables, TypeFamilies #-}

module System.Random.MRG32k3a
    (
      Gen
    , GenI
    , initialize

    , uniform

    , Seed
    , fromSeed
    , save
    , restore

    , jump
    ) where

-- import System.Random
import Data.Typeable (Typeable)
import Data.Word (Word32,Word64)
import Data.Int (Int64)
import Data.List (unfoldr)

import System.Random.MRG.Internal

class MRGen g where
  initialize :: (Integral a) => a -> g
  uniform :: g -> (Double,g)
  save :: g -> Seed
  restore :: Seed -> g

newtype Gen = Gen (Double,Double,Double,Double,Double,Double)

newtype GenI = GenI (Int64,Int64,Int64,Int64,Int64,Int64)

instance MRGen Gen where
  initialize = initializeD
  uniform gen = mrg32k3a_genRand gen
  save = saveD
  restore = restoreD
  {-# INLINE uniform #-}

instance MRGen GenI where
  initialize = initializeI
  uniform gen = mrg32k3a_genRandI gen
  save = saveI
  restore = restoreI
  {-# INLINE uniform #-}

norm :: Double
norm = 2.328306549295728e-10
{-# INLINE norm #-}

m1 :: Int64
m1 = 4294967087
{-# INLINE m1 #-}

m1f :: Double
m1f = fromIntegral m1
{-# INLINE m1f #-}

m2 :: Int64
m2 = 4294944443
{-# INLINE m2 #-}

m2f :: Double
m2f = fromIntegral m2
{-# INLINE m2f #-}

a12 :: Int64
a12 = 1403580
{-# INLINE a12 #-}

a12f :: Double
a12f = fromIntegral a12
{-# INLINE a12f #-}

a13n :: Int64
a13n = 810728
{-# INLINE a13n #-}

a13nf :: Double
a13nf = fromIntegral a13n
{-# INLINE a13nf #-}

a21 :: Int64
a21 = 527612
{-# INLINE a21 #-}

a21f :: Double
a21f = fromIntegral a21
{-# INLINE a21f #-}

a23n :: Int64
a23n = 1370589
{-# INLINE a23n #-}

a23nf :: Double
a23nf = fromIntegral a23n
{-# INLINE a23nf #-}

floorInt :: Double -> Int
floorInt = floor

mrg32k3a_genRand :: Gen -> (Double,Gen)
mrg32k3a_genRand (Gen (s10,s11,s12,s20,s21,s22))
  = (v, Gen (s11,s12,t1,s21,s22,t2))
  where p1 = a12f * s11 - a13nf * s10
        q1 = floorInt (p1 / m1f)
        r1 = p1 - fromIntegral q1 * m1f
        !t1 = if r1 < 0.0 then r1 + m1f else r1
        p2 = a21f * s22 - a23nf * s20
        q2 = floorInt (p2 / m2f)
        r2 = p2 - fromIntegral q2 * m2f
        !t2 = if r2 < 0.0 then r2 + m2f else r2
        !v = if t1 <= t2 then (t1 - t2 + m1f) * norm else (t1 - t2) * norm
{-# INLINE mrg32k3a_genRand #-}

mrg32k3a_genRandI :: GenI -> (Double,GenI)
mrg32k3a_genRandI (GenI (s10,s11,s12,s20,s21,s22))
  = (v, GenI (s11,s12,t1,s21,s22,t2))
  where p1 = a12 * s11 - a13n * s10
        !t1 = p1 `mod` m1
        p2 = a21 * s22 - a23n * s20
        !t2 = p2 `mod` m2
        t1f = fromIntegral t1
        t2f = fromIntegral t2
        !v = if t1f <= t2f then (t1f - t2f + m1f) * norm else (t1f - t2f) * norm
{-# INLINE mrg32k3a_genRandI #-}

initializeD :: (Integral a) => a -> Gen
initializeD seed = Gen (s1,s1,s1,s2,s2,s2)
  where s' = fromIntegral seed
        s1 = fromIntegral $ s' `mod` m1
        s2 = fromIntegral $ s' `mod` m2
{-# INLINE initializeD #-}

initializeI :: (Integral a) => a -> GenI
initializeI seed = GenI (s1,s1,s1,s2,s2,s2)
  where s' = fromIntegral seed
        s1 = s' `mod` m1
        s2 = s' `mod` m2
{-# INLINE initializeI #-}

newtype Seed = Seed { fromSeed :: (Word32,Word32,Word32,Word32,Word32,Word32) }
  deriving (Eq, Show, Typeable)

saveD :: Gen -> Seed
saveD (Gen (s10,s11,s12,s20,s21,s22)) = Seed (t1,t2,t3,t4,t5,t6)
  where t1 = floor s10
        t2 = floor s11
        t3 = floor s12
        t4 = floor s20
        t5 = floor s21
        t6 = floor s22
{-# INLINE saveD #-}

saveI :: GenI -> Seed
saveI (GenI (s10,s11,s12,s20,s21,s22)) = Seed (t1,t2,t3,t4,t5,t6)
  where t1 = fromIntegral s10
        t2 = fromIntegral s11
        t3 = fromIntegral s12
        t4 = fromIntegral s20
        t5 = fromIntegral s21
        t6 = fromIntegral s22
{-# INLINE saveI #-}

restoreD :: Seed -> Gen
restoreD (Seed (t1,t2,t3,t4,t5,t6)) = Gen (s10,s11,s12,s20,s21,s22)
  where m1' = fromIntegral m1
        m2' = fromIntegral m2
        s10 = fromIntegral $ t1 `mod` m1'
        s11 = fromIntegral $ t2 `mod` m1'
        s12 = fromIntegral $ t3 `mod` m1'
        s20 = fromIntegral $ t4 `mod` m2'
        s21 = fromIntegral $ t5 `mod` m2'
        s22 = fromIntegral $ t6 `mod` m2'
{-# INLINE restoreD #-}

restoreI :: Seed -> GenI
restoreI (Seed (t1,t2,t3,t4,t5,t6)) = GenI (s10,s11,s12,s20,s21,s22)
  where s10 = fromIntegral t1 `mod` m1
        s11 = fromIntegral t2 `mod` m1
        s12 = fromIntegral t3 `mod` m1
        s20 = fromIntegral t4 `mod` m2
        s21 = fromIntegral t5 `mod` m2
        s22 = fromIntegral t6 `mod` m2
{-# INLINE restoreI #-}

jump :: Int -> Gen -> Gen
jump e g@(Gen (s10,s11,s12,s20,s21,s22))
  | e > 64    = error "Jump factor must be smaller than 64."
  | e == 0    = g
  | otherwise = Gen (t10,t11,t12,t20,t21,t22)
  where m1' = fromIntegral m1 :: Int64
        m2' = fromIntegral m2 :: Int64
        v1 = floor <$> SV (s10, s11, s12)
        v2 = floor <$> SV (s20, s21, s22)
        (b1,b2) = jmtxs !! (e-1)
        w1 = vecTrOn m1' (fromIntegral <$> b1) v1
        w2 = vecTrOn m2' (fromIntegral <$> b2) v2
        SV (t10,t11,t12) = fromIntegral <$> w1
        SV (t20,t21,t22) = fromIntegral <$> w2

mtx1 :: JumpMatrix Word64
mtx1 = fromIntegral <$> JM (0, 1, 0) (0, 0, 1) (m1 - a13n, a12, 0)

mtx2 :: JumpMatrix Word64
mtx2 = fromIntegral <$> JM (0, 1, 0) (0, 0, 1) (m2 - a23n, 0, a21)

cntdn :: (a -> a) -> (a, Int) -> Maybe (a, (a, Int))
cntdn _ (_, 0) = Nothing
cntdn f (x, k) = Just (y, (y, k-1))
  where y = f x

jmtxs :: [(JumpMatrix Word32,JumpMatrix Word32)]
jmtxs = zip (map (fromIntegral <$>) xs) (map (fromIntegral <$>) ys)
  where n = 64
        mtx1' = fromIntegral <$> mtx1
        xs = unfoldr (cntdn (matSqrOn m1)) (mtx1',n)
        mtx2' = fromIntegral <$> mtx2
        ys = unfoldr (cntdn (matSqrOn m2)) (mtx2',n)

-- EOF
