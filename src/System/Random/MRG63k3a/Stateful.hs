{- -*- mode: haskell; coding: utf-8-unix -*- -}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
-- |
-- Module    : System.Random.MRG63k3a.Stateful
-- Copyright : (c) 2020 Naoyuki MORITA
-- License   : BSD3
--
-- Maintainer  : naoyuki.morita@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Pseudo-random number generation with MRG63k3a [\[1\]](#lecuyer1999) (monadic interface).
--
-- The generator type 'Gen' is an instance of 'StatefulGen' type class, so
-- it can be used through 'StatefulGen' intreface functions such like,
--
-- @
--   > gen \<- 'initialize' 1234567
--   > replicateM 10 ('uniformM' gen) :: IO [Word32]
--   [2246106302,1563963788,2439712072,3737154441,2667077669,767817191,747111673,2638409746,3331088863,4075662417]
--   > replicateM 10 ('uniformM' gen) :: IO [Word32]
--   [1456421684,2935764772,936846699,649810874,4215441082,311517124,1039486180,751453058,3053799799,1547236802]
-- @
--
module System.Random.MRG63k3a.Stateful
    (
    -- * Gen: Pseudo-Random Number Generators
      Gen
    , initialize

    -- ** Type helpers
    , GenIO
    , GenST

    -- * Unitility functions
    , uniform01M

    -- * Seed: state management
    -- $statemgmt
    , Seed
    , fromSeed

    -- * References
    -- $references
    ) where

import Control.Monad.Primitive (PrimMonad, PrimState, unsafeSTToPrim)
import Control.Monad.ST        (ST)
import qualified Data.Primitive.PrimArray as P
import Data.Typeable           (Typeable)
import Data.Int                (Int64)
import Data.Word
import Data.Bits               ((.&.),(.|.),shiftL)

import System.Random.MRG.Internal
import System.Random.MRG63k3a.Internal

import System.Random.Stateful

-- | State of the pseudo-random number generator. It uses mutable
-- state so same generator shouldn't be used from the different
-- threads simultaneously.
newtype Gen s = Gen (P.MutablePrimArray s Int64)

-- | A shorter name for PRNG state in the 'IO' monad.
type GenIO = Gen (PrimState IO)

-- | A shorter name for PRNG state in the 'ST' monad.
type GenST s = Gen (PrimState (ST s))

instance (s ~ PrimState m, PrimMonad m) => StatefulGen (Gen s) m where
  uniformWord32 = uniformW32
  uniformWord16 = uniformW16
  uniformWord8  = uniformW8
  uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n go)
    where go = do
            x0 <- uniformW32 g
            x1 <- uniformW32 g
            pure $ (fromIntegral x0 `shiftL` 32) .|. fromIntegral x1
  {-# INLINE uniformWord32 #-}
  {-# INLINE uniformWord16 #-}
  {-# INLINE uniformWord8  #-}
  {-# INLINE uniformShortByteString #-}

instance (PrimMonad m) => FrozenGen Seed m where
  type MutableGen Seed m = Gen (PrimState m)
  thawGen = restore
  freezeGen = save

readState :: (PrimMonad m) => P.MutablePrimArray (PrimState m) Int64 -> m (Int64,Int64,Int64,Int64,Int64,Int64)
readState ary = do
    s10 <- P.readPrimArray ary 0
    s11 <- P.readPrimArray ary 1
    s12 <- P.readPrimArray ary 2
    s20 <- P.readPrimArray ary 3
    s21 <- P.readPrimArray ary 4
    s22 <- P.readPrimArray ary 5
    return (s10,s11,s12,s20,s21,s22)
{-# INLINE readState #-}

writeState :: (PrimMonad m) => P.MutablePrimArray (PrimState m) Int64 -> (Int64,Int64,Int64,Int64,Int64,Int64) -> m ()
writeState ary (s10,s11,s12,s20,s21,s22) = do
    P.writePrimArray ary 0 s10
    P.writePrimArray ary 1 s11
    P.writePrimArray ary 2 s12
    P.writePrimArray ary 3 s20
    P.writePrimArray ary 4 s21
    P.writePrimArray ary 5 s22
{-# INLINE writeState #-}

mrg63k3a_genRand :: (PrimMonad m) => Gen (PrimState m) -> m Int64
mrg63k3a_genRand (Gen ary) = do
    s@(_,s11,s12,_,s21,s22) <- readState ary
    let (!t1,!t2) = mrg63k3a_rec s
        v = if t1 > t2 then t1 - t2 else t1 - t2 + m1
    writeState ary (s11,s12,t1,s21,s22,t2)
    return v
{-# INLINE mrg63k3a_genRand #-}

-- | Get a random value following U(0,1).
uniform01M :: (PrimMonad m) => Gen (PrimState m) -> m Double
uniform01M g = (* norm) <$> fromIntegral <$> mrg63k3a_genRand g
{-# INLINE uniform01M #-}

ub32 :: Int64
ub32 = v
  where !r = m1 `mod` 4294967296
        !v = m1 - r
{-# INLINE ub32 #-}

uniformW32 :: (PrimMonad m) => Gen (PrimState m) -> m Word32
uniformW32 gen = go
  where go = do
          v <- mrg63k3a_genRand gen
          if v >= ub32 then go else return (fromIntegral (v .&. 4294967295))
{-# INLINE uniformW32 #-}

ub16 :: Int64
ub16 = v
  where !r = m1 `mod` 65536
        !v = m1 - r
{-# INLINE ub16 #-}

uniformW16 :: (PrimMonad m) => Gen (PrimState m) -> m Word16
uniformW16 gen = go
  where go = do
          v <- mrg63k3a_genRand gen
          if v >= ub16 then go else return (fromIntegral (v .&. 65535))
{-# INLINE uniformW16 #-}

ub8 :: Int64
ub8 = v
  where !r = m1 `mod` 256
        !v = m1 - r
{-# INLINE ub8 #-}

uniformW8 :: (PrimMonad m) => Gen (PrimState m) -> m Word8
uniformW8 gen = go
  where go = do
          v <- mrg63k3a_genRand gen
          if v >= ub8 then go else return (fromIntegral (v .&. 255))
{-# INLINE uniformW8 #-}

-- | Create a generator using given seed.
initialize :: (PrimMonad m) => Int64 -> m (Gen (PrimState m))
initialize seed = do
    let s1 = seed `mod` m1
        s2 = seed `mod` m2
    ary <- P.newPrimArray 6
    writeState ary (s1,s1,s1,s2,s2,s2)
    return $ Gen ary
{-# INLINE initialize #-}

-- | An immutable snapshot of the state of a 'Gen'.
newtype Seed = Seed {
  -- | Convert seed into a 6-tuple of @Word64@.
  fromSeed :: (Word64,Word64,Word64,Word64,Word64,Word64)
  }
  deriving (Eq, Show, Typeable)

save :: (PrimMonad m) => Gen (PrimState m) -> m Seed
save (Gen ary) = do
    st <- readState ary
    return $ Seed $ fromT6 $ fromIntegral <$> (T6 st)
{-# INLINE save #-}

restore :: (PrimMonad m) => Seed -> m (Gen (PrimState m))
restore (Seed (t1,t2,t3,t4,t5,t6)) = do
    let m1' = fromIntegral m1
        m2' = fromIntegral m2
        st = T6 (t1 `mod` m1', t2 `mod` m1', t3 `mod` m1',
                 t4 `mod` m2', t5 `mod` m2', t6 `mod` m2')
    ary <- P.newPrimArray 6
    writeState ary (fromT6 (fromIntegral <$> st))
    return $ Gen ary
{-# INLINE restore #-}

-- $statemgmt
--
-- You can get the current PRNG state by 'freezeGen' as an immutable data
-- that has type 'Seed'. You may save the state into persistent store and
-- restore the state by 'thawGen' later.
--
-- @
--   > gen \<- 'initialize' 1234567
--   > replicateM 10 ('uniform01M' gen)
--   [0.9964374245717021,0.9073161749933566,0.308369875218738,0.2928356495081096,0.6407970127747293,0.8444224582886195,0.1358954027173811,0.4542392932876494,0.8016794723344877,0.8370627714083252]
--   > seed \<- 'freezeGen' gen
--   > show $ 'fromSeed' seed
--   "(8956691725955036650,4881994573324246905,544766949767175019,4767073730205058520,6711178582528615333,2047597627722241317)"
--   > replicateM 10 ('uniform01M' gen)
--   [0.43854909685398463,0.3675952030734795,0.9681374152275398,0.7952475446049576,0.645021516355446,0.3490345515648514,0.13967842526828145,0.6463610214064653,0.3197503428491851,0.40268376160424424]
-- @
--
-- (in another context,)
--
-- @
--   > let seed = 'Seed' $ read "(8956691725955036650,4881994573324246905,544766949767175019,4767073730205058520,6711178582528615333,2047597627722241317)"
--   > gen \<- 'thawGen' seed
--   > replicateM 10 ('uniform01M' gen)
--   [0.43854909685398463,0.3675952030734795,0.9681374152275398,0.7952475446049576,0.645021516355446,0.3490345515648514,0.13967842526828145,0.6463610214064653,0.3197503428491851,0.40268376160424424]
-- @
--

-- $references
--
-- #lecuyer1999# [1] Pierre L'Ecuyer,  (1999) Good Parameters and Implementations for
-- Combined Multiple Recursive Random Number Generators.Operations Research 47(1):159-164.
-- <https://doi.org/10.1287/opre.47.1.159>

-- EOF
