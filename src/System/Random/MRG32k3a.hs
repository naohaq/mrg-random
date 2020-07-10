{- -*- mode: haskell; coding: utf-8-unix -*- -}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveDataTypeable       #-}
-- |
-- Module    : System.Random.MRG32k3a
-- Copyright : (c) 2020 Naoyuki MORITA
-- License   : BSD3
--
-- Maintainer  : naoyuki.morita@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Pseudo-random number generation with MRG(Multiple Recursive Generator).
--
module System.Random.MRG32k3a
    (
    -- * Gen: Pseudo-Random Number Generators
      Gen
    , initialize

    -- * Unitility functions
    , uniform01

    -- * Seed: state management
    , Seed
    , fromSeed
    , save
    , restore

    -- * Stream jumping
    , jump
    ) where

import Data.Typeable (Typeable)
import Data.Word (Word32,Word64)
import Data.Bits ((.&.))

import System.Random

import System.Random.MRG.Internal
import System.Random.MRG32k3a.Internal

newtype Gen = Gen (Double,Double,Double,Double,Double,Double)

instance RandomGen Gen where
  genWord32 = uniformW32
  split _ = error "Not yet implemented."

mrg32k3a_genRand :: Gen -> (Double,Gen)
mrg32k3a_genRand (Gen s@(_ ,s11,s12,_ ,s21,s22))
  = (v, Gen (s11,s12,t1,s21,s22,t2))
  where (t1,t2) = mrg32k3a_rec s
        v = if t1 <= t2 then t1 - t2 + m1f else t1 - t2
{-# INLINE mrg32k3a_genRand #-}

uniform01 :: Gen -> (Double,Gen)
uniform01 g = (w, g')
  where !w = v * norm
        (!v,!g') = mrg32k3a_genRand g
{-# INLINE uniform01 #-}

ub :: Word64
ub = v
  where !m1sq = m1 * m1
        !r = m1sq `mod` 4294967296
        !v = m1sq - r
{-# INLINE ub #-}

uniformW32 :: Gen -> (Word32,Gen)
uniformW32 g = if x >= ub
               then uniformW32 g1
               else (fromIntegral (x .&. 4294967295), g1)
  where (v0,g0) = mrg32k3a_genRand g
        (v1,g1) = mrg32k3a_genRand g0
        w0 = floor v0 :: Word64
        w1 = floor v1 :: Word64
        x = w0 * m1 + w1
{-# INLINE uniformW32 #-}

initialize :: (Integral a) => a -> Gen
initialize seed = Gen (s1,s1,s1,s2,s2,s2)
  where s' = fromIntegral seed
        s1 = fromIntegral $ s' `mod` m1
        s2 = fromIntegral $ s' `mod` m2
{-# INLINE initialize #-}

newtype Seed = Seed { fromSeed :: (Word32,Word32,Word32,Word32,Word32,Word32) }
  deriving (Eq, Show, Typeable)

save :: Gen -> Seed
save (Gen s) = Seed (fromT6 t)
  where t = floor <$> T6 s
{-# INLINE save #-}

restore :: Seed -> Gen
restore (Seed (t1,t2,t3,t4,t5,t6)) = Gen (fromT6 s)
  where m1' = fromIntegral m1
        m2' = fromIntegral m2
        t   = T6 (t1 `mod` m1', t2 `mod` m1', t3 `mod` m1',
                  t4 `mod` m2', t5 `mod` m2', t6 `mod` m2')
        s   = fromIntegral <$> t
{-# INLINE restore #-}

jump :: Int -> Gen -> Gen
jump e (Gen (s10,s11,s12,s20,s21,s22))
  | e > 64 || e < 0 = error "Jump factor must be in the range of [0,64]."
  | otherwise       = Gen (t10,t11,t12,t20,t21,t22)
  where m1' = fromIntegral m1 :: Word64
        m2' = fromIntegral m2 :: Word64
        v1 = floor <$> SV (s10, s11, s12)
        v2 = floor <$> SV (s20, s21, s22)
        (b1,b2) = jmtxs !! e
        w1 = vecTrMod m1' (fromIntegral <$> b1) v1
        w2 = vecTrMod m2' (fromIntegral <$> b2) v2
        SV (!t10,!t11,!t12) = fromIntegral <$> w1
        SV (!t20,!t21,!t22) = fromIntegral <$> w2

-- EOF
