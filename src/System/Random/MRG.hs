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
import Data.Word (Word32)
-- import qualified Data.Vector.Unboxed as I

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

a12 :: Double
a12 = 1403580
{-# INLINE a12 #-}

a13n :: Double
a13n = 810728
{-# INLINE a13n #-}

a21 :: Double
a21 = 527612
{-# INLINE a21 #-}

a23n :: Double
a23n = 1370589
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


-- EOF
