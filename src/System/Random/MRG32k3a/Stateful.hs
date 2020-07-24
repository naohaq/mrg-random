{- -*- mode: haskell; coding: utf-8-unix -*- -}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
-- |
-- Module    : System.Random.MRG32k3a.Stateful
-- Copyright : (c) 2020 Naoyuki MORITA
-- License   : BSD3
--
-- Maintainer  : naoyuki.morita@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Pseudo-random number generation with MRG32k3a [\[1\]](#lecuyer1999) (monadic interface).
--
-- The generator type 'Gen' is an instance of 'StatefulGen' type class, so
-- it can be used through 'StatefulGen' intreface functions such like,
--
-- @
--   > gen \<- 'initialize' 12345
--   > replicateM 10 ('uniformM' gen) :: IO [Word32]
--   [3320887301,884645991,913733706,3752754456,1435703647,511136538,160968264,1722383799,2355335811,690051551]
--   > replicateM 10 ('uniformM' gen) :: IO [Word32]
--   [2969339749,1695849133,1356079654,2439569495,4195450174,664833117,1860624843,3302720735,962971476,547363651]
-- @
--
-- __Notice:__ MRG32k3a is originally designed to generate random numbers
-- following U(0,1). It DOES NOT generate exactly 32-bit information at a
-- time.
--
-- If you need U(0,1) random numbers, use 'uniform01M' that generates a
-- random value efficiently by original MRG32k3a algorithm.
--
module System.Random.MRG32k3a.Stateful
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
import Data.Word
import Data.Bits               ((.&.),(.|.),shiftL)

import System.Random.MRG.Internal
import System.Random.MRG32k3a.Internal

import System.Random.Stateful

-- | State of the pseudo-random number generator. It uses mutable
-- state so same generator shouldn't be used from the different
-- threads simultaneously.
newtype Gen s = Gen (P.MutablePrimArray s Double)

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

instance (PrimMonad m) => FrozenGen Seed m where
  type MutableGen Seed m = Gen (PrimState m)
  thawGen = restore
  freezeGen = save

readState :: (PrimMonad m) => P.MutablePrimArray (PrimState m) Double -> m (Double,Double,Double,Double,Double,Double)
readState ary = do
    s10 <- P.readPrimArray ary 0
    s11 <- P.readPrimArray ary 1
    s12 <- P.readPrimArray ary 2
    s20 <- P.readPrimArray ary 3
    s21 <- P.readPrimArray ary 4
    s22 <- P.readPrimArray ary 5
    return (s10,s11,s12,s20,s21,s22)
{-# INLINE readState #-}

writeState :: (PrimMonad m) => P.MutablePrimArray (PrimState m) Double -> (Double,Double,Double,Double,Double,Double) -> m ()
writeState ary (s10,s11,s12,s20,s21,s22) = do
    P.writePrimArray ary 0 s10
    P.writePrimArray ary 1 s11
    P.writePrimArray ary 2 s12
    P.writePrimArray ary 3 s20
    P.writePrimArray ary 4 s21
    P.writePrimArray ary 5 s22
{-# INLINE writeState #-}

mrg32k3a_genRand :: (PrimMonad m) => Gen (PrimState m) -> m Double
mrg32k3a_genRand (Gen ary) = do
    s@(_,s11,s12,_,s21,s22) <- readState ary
    let (!t1,!t2) = mrg32k3a_rec s
        v = if t1 <= t2 then t1 - t2 + m1f else t1 - t2
    writeState ary (s11,s12,t1,s21,s22,t2)
    return v
{-# INLINE mrg32k3a_genRand #-}

-- | Get a random value following U(0,1).
uniform01M :: (PrimMonad m) => Gen (PrimState m) -> m Double
uniform01M g = (* norm) <$> mrg32k3a_genRand g
{-# INLINE uniform01M #-}

ub32 :: Word64
ub32 = m1sq - r
  where !m1sq = m1 * m1
        !r = m1sq `mod` 4294967296
{-# INLINE ub32 #-}

uniformW32 :: (PrimMonad m) => Gen (PrimState m) -> m Word32
uniformW32 gen = go
  where go = do
          v0 <- floor <$> mrg32k3a_genRand gen
          v1 <- floor <$> mrg32k3a_genRand gen
          let x = v0 * m1 + v1
          if x >= ub32 then go else return (fromIntegral (x .&. 4294967295))
{-# INLINE uniformW32 #-}

ub16 :: Word64
ub16 = v
  where !r = m1 `mod` 65536
        !v = m1 - r
{-# INLINE ub16 #-}

uniformW16 :: (PrimMonad m) => Gen (PrimState m) -> m Word16
uniformW16 gen = go
  where go = do
          x <- floor <$> mrg32k3a_genRand gen
          if x >= ub16 then go else return (fromIntegral (x .&. 65535))
{-# INLINE uniformW16 #-}

ub8 :: Word64
ub8 = v
  where !r = m1 `mod` 256
        !v = m1 - r
{-# INLINE ub8 #-}

uniformW8 :: (PrimMonad m) => Gen (PrimState m) -> m Word8
uniformW8 gen = go
  where go = do
          x <- floor <$> mrg32k3a_genRand gen
          if x >= ub8 then go else return (fromIntegral (x .&. 255))
{-# INLINE uniformW8 #-}

-- | Create a generator using given seed.
initialize :: (PrimMonad m) => Word32 -> m (Gen (PrimState m))
initialize seed = do
    let s' = fromIntegral seed
        s1 = fromIntegral $ s' `mod` m1
        s2 = fromIntegral $ s' `mod` m2
    ary <- P.newPrimArray 6
    writeState ary (s1,s1,s1,s2,s2,s2)
    return $ Gen ary
{-# INLINE initialize #-}

-- | An immutable snapshot of the state of a 'Gen'.
newtype Seed = Seed {
  -- | Convert seed into a 6-tuple of @Word32@.
  fromSeed :: (Word32,Word32,Word32,Word32,Word32,Word32)
  }
  deriving (Eq, Show, Typeable)

save :: (PrimMonad m) => Gen (PrimState m) -> m Seed
save (Gen ary) = do
    st <- readState ary
    return $ Seed $ fromT6 $ floor <$> (T6 st)
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
--   > gen \<- 'initialize' 12345
--   > replicateM 10 ('uniform01M' gen)
--   [0.12701112204657714,0.3185275653967945,0.3091860155832701,0.8258468629271136,0.2216299157820229,0.5333953879182788,0.4807742033156181,0.3555598794381262,0.13598841039594017,0.7558522371615436]
--   > seed \<- 'freezeGen' gen
--   > show $ 'fromSeed' seed
--   "(2989318136,3378525425,1773647758,1462200156,2794459678,2822254363)"
--   > replicateM 10 ('uniform01M' gen)
--   [0.5755553189002691,0.4100640936040626,0.3263296794324586,0.24037805455705044,0.6100629823964789,0.9041809183707534,0.2989749433907653,3.415449711124772e-2,0.9664250719399228,0.1434954073855292]
-- @
--
-- (in another context,)
--
-- @
--   > let seed = 'Seed' $ read "(2989318136,3378525425,1773647758,1462200156,2794459678,2822254363)"
--   > gen \<- 'thawGen' seed
--   > replicateM 10 ('uniform01M' gen)
--   [0.5755553189002691,0.4100640936040626,0.3263296794324586,0.24037805455705044,0.6100629823964789,0.9041809183707534,0.2989749433907653,3.415449711124772e-2,0.9664250719399228,0.1434954073855292]
-- @
--

-- $references
--
-- #lecuyer1999# [1] Pierre L'Ecuyer,  (1999) Good Parameters and Implementations for
-- Combined Multiple Recursive Random Number Generators.Operations Research 47(1):159-164.
-- <https://doi.org/10.1287/opre.47.1.159>

-- EOF
