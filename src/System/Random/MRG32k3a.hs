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
-- Pseudo-random number generation with MRG32k3a.
--
-- The generator type 'Gen' is an instance of 'RandomGen' type class, so
-- it can be used through 'RandomGen' intreface functions such like,
--
-- @
--   > let g = 'initialize' 12345
--   > let (x, g') = 'uniform' g :: (Word32, Gen) in x
--   3320887301
-- @
--
-- __Notice:__ MRG32k3a is originally designed to generate random numbers
-- following U(0,1). It DOES NOT generate exactly 32-bit information at a
-- time.
--
-- If you need U(0,1) random numbers, use 'uniform01' that generates a
-- random value efficiently by original MRG32k3a algorithm.
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
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits ((.&.))

import System.Random

import System.Random.MRG.Internal
import System.Random.MRG32k3a.Internal

-- | The generator type.
newtype Gen = Gen (Double,Double,Double,Double,Double,Double)

instance RandomGen Gen where
  genWord32 = uniformW32
  genWord16 = uniformW16
  genWord8  = uniformW8
  split _ = error "Not yet implemented."
  {-# INLINE genWord32 #-}
  {-# INLINE genWord16 #-}
  {-# INLINE genWord8  #-}

mrg32k3a_genRand :: Gen -> (Double,Gen)
mrg32k3a_genRand (Gen s@(_ ,s11,s12,_ ,s21,s22))
  = (v, Gen (s11,s12,t1,s21,s22,t2))
  where (t1,t2) = mrg32k3a_rec s
        v = if t1 <= t2 then t1 - t2 + m1f else t1 - t2
{-# INLINE mrg32k3a_genRand #-}

-- | Get a random value following U(0,1).
uniform01 :: Gen -> (Double,Gen)
uniform01 g = (w, g')
  where !w = v * norm
        (!v,!g') = mrg32k3a_genRand g
{-# INLINE uniform01 #-}

ub32 :: Word64
ub32 = v
  where !m1sq = m1 * m1
        !r = m1sq `mod` 4294967296
        !v = m1sq - r
{-# INLINE ub32 #-}

uniformW32 :: Gen -> (Word32,Gen)
uniformW32 g = if x >= ub32
               then uniformW32 g1
               else (fromIntegral (x .&. 4294967295), g1)
  where (v0,g0) = mrg32k3a_genRand g
        (v1,g1) = mrg32k3a_genRand g0
        w0 = floor v0 :: Word64
        w1 = floor v1 :: Word64
        x = w0 * m1 + w1
{-# INLINE uniformW32 #-}

ub16 :: Word64
ub16 = v
  where !r = m1 `mod` 65536
        !v = m1 - r
{-# INLINE ub16 #-}

uniformW16 :: Gen -> (Word16,Gen)
uniformW16 gen = go gen
  where go g = if x >= ub16
               then go g'
               else (y, g')
          where (v, g') = mrg32k3a_genRand g
                x = floor v :: Word64
                !y = fromIntegral (x .&. 65535)
{-# INLINE uniformW16 #-}

ub8 :: Word64
ub8 = v
  where !r = m1 `mod` 256
        !v = m1 - r
{-# INLINE ub8 #-}

uniformW8 :: Gen -> (Word8,Gen)
uniformW8 gen = go gen
  where go g = if x >= ub8
               then go g'
               else (y, g')
          where (v, g') = mrg32k3a_genRand g
                x = floor v :: Word64
                !y = fromIntegral (x .&. 255)
{-# INLINE uniformW8 #-}

-- | Create a generator using given seed.
initialize :: Word32 -> Gen
initialize seed = Gen (s1,s1,s1,s2,s2,s2)
  where s' = fromIntegral seed
        s1 = fromIntegral $ s' `mod` m1
        s2 = fromIntegral $ s' `mod` m2
{-# INLINE initialize #-}

-- | An immutable snapshot of the state of a 'Gen'.
newtype Seed = Seed {
  -- | Convert seed into a 6-tuple of @Word32@.
  fromSeed :: (Word32,Word32,Word32,Word32,Word32,Word32)
  }
  deriving (Eq, Show, Typeable)

-- | Save the state of a 'Gen'. Saved state can be used by 'restore'.
save :: Gen -> Seed
save (Gen s) = Seed (fromT6 t)
  where t = floor <$> T6 s
{-# INLINE save #-}

-- | Create a new 'Gen' that mirrors the state of a saved 'Seed'.
restore :: Seed -> Gen
restore (Seed (t1,t2,t3,t4,t5,t6)) = Gen (fromT6 s)
  where m1' = fromIntegral m1
        m2' = fromIntegral m2
        t   = T6 (t1 `mod` m1', t2 `mod` m1', t3 `mod` m1',
                  t4 `mod` m2', t5 `mod` m2', t6 `mod` m2')
        s   = fromIntegral <$> t
{-# INLINE restore #-}

-- | Get a new generator jumps ahead by \(2^n\) steps from given generator.
--
-- @
--   > let g0 = 'initialize' 12345
--   > let g1 = 'jump' 20 g0
--   > let xs = unfoldr (Just . 'uniform01') g0
--   > let ys = unfoldr (Just . 'uniform01') g1
--   > take 10 $ drop 1048576 xs
--   [0.42963674510001276,0.10482156807623948,0.9889648413995019,0.785875227875553,0.9522150221887802,0.9792979233185687,0.8713777766671446,0.9231321178403405,0.13047652927672448,0.5395971153015737]
--   > take 10 $ ys
--   [0.42963674510001276,0.10482156807623948,0.9889648413995019,0.785875227875553,0.9522150221887802,0.9792979233185687,0.8713777766671446,0.9231321178403405,0.13047652927672448,0.5395971153015737]
-- @
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
