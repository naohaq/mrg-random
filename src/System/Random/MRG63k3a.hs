{- -*- mode: haskell; coding: utf-8-unix -*- -}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveDataTypeable       #-}
-- |
-- Module    : System.Random.MRG63k3a
-- Copyright : (c) 2020 Naoyuki MORITA
-- License   : BSD3
--
-- Maintainer  : naoyuki.morita@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Pseudo-random number generation with MRG63k3a.
--
-- The generator type 'Gen' is an instance of 'RandomGen' type class, so
-- it can be used through 'RandomGen' intreface functions such like,
--
-- @
--   >>> let g = 'initialize' 1234567
--   >>> let (x, g') = 'uniform' g :: (Word32, Gen) in x
--   2246106302
-- @
--
module System.Random.MRG63k3a
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
import Data.Int  (Int64)
import Data.Word (Word32,Word64)
import Data.Bits ((.&.))

import System.Random

import System.Random.MRG.Internal
import System.Random.MRG63k3a.Internal

-- | The generator type.
data Gen = Gen (Int64,Int64,Int64,Int64,Int64,Int64)

instance RandomGen Gen where
  genWord32 = uniformW32
  split _ = error "Not yet implemented."
  {-# INLINE genWord32 #-}

mrg63k3a_genRand :: Gen -> (Int64,Gen)
mrg63k3a_genRand (Gen s@(_ ,s11,s12,_ ,s21,s22))
  = (v, Gen (s11,s12,t1,s21,s22,t2))
  where (t1,t2) = mrg63k3a_rec s
        v = if t1 > t2 then t1 - t2 else t1 - t2 + m1
{-# INLINE mrg63k3a_genRand #-}

-- | Create a generator using given seed.
initialize :: Int64 -> Gen
initialize seed = Gen (s1,s1,s1,s2,s2,s2)
  where s1 = seed `mod` m1
        s2 = seed `mod` m2
{-# INLINE initialize #-}

-- | Get a random value following U(0,1).
uniform01 :: Gen -> (Double, Gen)
uniform01 gen = (w,gen')
  where (v,gen') = mrg63k3a_genRand gen
        !w = norm * fromIntegral v
{-# INLINE uniform01 #-}

ub :: Int64
ub = v
  where !r = m1 `mod` 4294967296
        !v = m1 - r
{-# INLINE ub #-}

uniformW32 :: Gen -> (Word32, Gen)
uniformW32 gen = go gen
  where go g = if x >= ub then go g' else (fromIntegral (x .&. 4294967295), g')
          where (x,g') = mrg63k3a_genRand g
{-# INLINE uniformW32 #-}

-- | An immutable snapshot of the state of a 'Gen'.
newtype Seed = Seed {
  -- | Convert seed into a 6-tuple of @Word64@.
  fromSeed :: (Word64,Word64,Word64,Word64,Word64,Word64)
  }
  deriving (Eq, Show, Typeable)

-- | Save the state of a 'Gen'. Saved state can be used by 'restore'.
save :: Gen -> Seed
save (Gen s) = Seed (fromT6 t)
  where t = fromIntegral <$> T6 s
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
--   >>> let g0 = initialize (1234567 :: Int)
--   >>> let g1 = jump 20 g0
--   >>> let xs = unfoldr (Just . uniform01) g0
--   >>> let ys = unfoldr (Just . uniform01) g1
--   >>> take 10 $ drop 1048576 xs
--   [0.8661862534637614,0.32299545841103733,0.7545203812431158,0.6829503373808978,0.7938803872652808,0.3453801891716969,0.199924353479426,0.7548724584148777,0.4533723232806143,0.855651888940174]
--   >>> take 10 ys
--   [0.8661862534637614,0.32299545841103733,0.7545203812431158,0.6829503373808978,0.7938803872652808,0.3453801891716969,0.199924353479426,0.7548724584148777,0.4533723232806143,0.855651888940174]
-- @
jump :: Int -> Gen -> Gen
jump e (Gen (s10,s11,s12,s20,s21,s22))
  | e > 64 || e < 0 = error "Jump factor must be in the range of [0,64]"
  | otherwise       = Gen (t10,t11,t12,t20,t21,t22)
  where m1' = fromIntegral m1
        m2' = fromIntegral m2
        v1  = fromIntegral <$> SV (s10, s11, s12)
        v2  = fromIntegral <$> SV (s20, s21, s22)
        (b1,b2) = jmtxs !! e
        w1  = vecTrModW64 m1' b1 v1
        w2  = vecTrModW64 m2' b2 v2
        SV (!t10,!t11,!t12) = fromIntegral <$> w1
        SV (!t20,!t21,!t22) = fromIntegral <$> w2

-- EOF
