{- -*- mode: haskell; coding: utf-8-unix -*-  -}
{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, FlexibleContexts,
    ScopedTypeVariables, TypeFamilies #-}

module System.Random.MRG
    (
      MRGen
    , initialize

    , uniform

    , Seed
    , fromSeed
    , save
    , restore
    ) where

-- import System.Random
import Data.Typeable (Typeable)
import Data.Word (Word32,Word64)
import Data.List (unfoldr)

import System.Random.MRG.Internal

data MRGen = MRGen Double Double Double Double Double Double

norm :: Double
norm = 2.328306549295728e-10
{-# INLINE norm #-}

m1 :: Int
m1 = 4294967087
{-# INLINE m1 #-}

m1f :: Double
m1f = fromIntegral m1
{-# INLINE m1f #-}

m2 :: Int
m2 = 4294944443
{-# INLINE m2 #-}

m2f :: Double
m2f = fromIntegral m2
{-# INLINE m2f #-}

ia12 :: Int
ia12 = 1403580
{-# INLINE ia12 #-}

a12 :: Double
a12 = fromIntegral ia12
{-# INLINE a12 #-}

ia13n :: Int
ia13n = 810728
{-# INLINE ia13n #-}

a13n :: Double
a13n = fromIntegral ia13n
{-# INLINE a13n #-}

ia21 :: Int
ia21 = 527612
{-# INLINE ia21 #-}

a21 :: Double
a21 = fromIntegral ia21
{-# INLINE a21 #-}

ia23n :: Int
ia23n = 1370589 
{-# INLINE ia23n #-}

a23n :: Double
a23n = fromIntegral ia23n
{-# INLINE a23n #-}

floorInt :: Double -> Int
floorInt = floor

mrg32k3a_genRand :: MRGen -> (Double,MRGen)
mrg32k3a_genRand (MRGen s10 s11 s12 s20 s21 s22)
  = (v, MRGen s11 s12 t1 s21 s22 t2)
  where p1 = a12 * s11 - a13n * s10
        q1 = floorInt (p1 / m1f)
        r1 = p1 - fromIntegral q1 * m1f
        !t1 = if r1 < 0.0 then r1 + m1f else r1
        p2 = a21 * s22 - a23n * s20
        q2 = floorInt (p2 / m2f)
        r2 = p2 - fromIntegral q2 * m2f
        !t2 = if r2 < 0.0 then r2 + m2f else r2
        !v = if t1 <= t2 then (t1 - t2 + m1f) * norm else (t1 - t2) * norm
{-# INLINE mrg32k3a_genRand #-}

initialize :: (Integral a) => a -> MRGen
initialize seed = MRGen s1 s1 s1 s2 s2 s2
  where s1 = fromIntegral $ seed `mod` fromIntegral m1
        s2 = fromIntegral $ seed `mod` fromIntegral m2
{-# INLINE initialize #-}

uniform :: MRGen -> (Double,MRGen)
uniform gen = mrg32k3a_genRand gen
{-# INLINE uniform #-}

newtype Seed = Seed { fromSeed :: (Word32,Word32,Word32,Word32,Word32,Word32) } deriving (Eq, Show, Typeable)

save :: MRGen -> Seed
save (MRGen s10 s11 s12 s20 s21 s22) = Seed (t1,t2,t3,t4,t5,t6)
  where t1 = floor s10
        t2 = floor s11
        t3 = floor s12
        t4 = floor s20
        t5 = floor s21
        t6 = floor s22
{-# INLINE save #-}

restore :: Seed -> MRGen
restore (Seed (t1,t2,t3,t4,t5,t6)) = MRGen s10 s11 s12 s20 s21 s22
  where s10 = fromIntegral $ t1 `mod` fromIntegral m1
        s11 = fromIntegral $ t2 `mod` fromIntegral m1
        s12 = fromIntegral $ t3 `mod` fromIntegral m1
        s20 = fromIntegral $ t4 `mod` fromIntegral m2
        s21 = fromIntegral $ t5 `mod` fromIntegral m2
        s22 = fromIntegral $ t6 `mod` fromIntegral m2
{-# INLINE restore #-}

jump :: Int -> MRGen -> MRGen
jump e (MRGen s10 s11 s12 s20 s21 s22) = MRGen t10 t11 t12 t20 t21 t22
  where m1' = fromIntegral m1 :: Word64
        m2' = fromIntegral m2 :: Word64
        v1 = floor <$> SV (s10, s11, s12)
        v2 = floor <$> SV (s20, s21, s22)
        (b1,b2) = jmtxs !! (e-1)
        w1 = vecTrOn m1' (fromIntegral <$> b1) v1
        w2 = vecTrOn m2' (fromIntegral <$> b2) v2
        SV (t10,t11,t12) = fromIntegral <$> w1
        SV (t20,t21,t22) = fromIntegral <$> w2

mtx1 :: JumpMatrix Word64
mtx1 = JM (0, 1, 0) (0, 0, 1) (fromIntegral m1 - fromIntegral ia13n, fromIntegral ia12, 0)

mtx2 :: JumpMatrix Word64
mtx2 = JM (0, 1, 0) (0, 0, 1) (fromIntegral m2 - fromIntegral ia23n, 0, fromIntegral ia21)

cntdn :: (a -> a) -> (a, Int) -> Maybe (a, (a, Int))
cntdn _ (_, 0) = Nothing
cntdn f (x, k) = Just (y, (y, k-1))
  where y = f x

jmtxs :: [(JumpMatrix Word32,JumpMatrix Word32)]
jmtxs = zip (map (fmap fromIntegral) xs) (map (fmap fromIntegral) ys)
  where n = 64
        m1' = fromIntegral m1 :: Word64
        mtx1' = fromIntegral <$> mtx1
        xs = unfoldr (cntdn (matSqrOn m1')) (mtx1',n)
        m2' = fromIntegral m2 :: Word64
        mtx2' = fromIntegral <$> mtx2
        ys = unfoldr (cntdn (matSqrOn m2')) (mtx2',n)

-- EOF
