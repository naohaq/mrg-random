{- -*- mode: haskell; coding: utf-8-unix -*-  -}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies #-}

module System.Random.MRG32k3a.Stateful
    (
      Gen
    , initialize
    , uniformDouble01M

    , Seed
    , fromSeed
    ) where

import Control.Monad           (liftM)
import Control.Monad.Primitive (PrimMonad, PrimState, unsafeSTToPrim)
import qualified Data.Primitive.PrimArray as P
import Data.Typeable           (Typeable)
import Data.Word
import Data.Bits               ((.&.),(.|.),shiftL)

import System.Random.Stateful hiding (uniformDouble01M)

newtype Gen s = Gen (P.MutablePrimArray s Double)

instance (s ~ PrimState m, PrimMonad m) => StatefulGen (Gen s) m where
  uniformWord32 = uniformW32
  uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n go)
    where go = do
            x0 <- uniformW32 g
            x1 <- uniformW32 g
            pure $ (fromIntegral x0 `shiftL` 32) .|. fromIntegral x1

instance (PrimMonad m) => FrozenGen Seed m where
  type MutableGen Seed m = Gen (PrimState m)
  thawGen = restore
  freezeGen = save

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

mrg32k3a_genRand :: (PrimMonad m) => Gen (PrimState m) -> m Double
mrg32k3a_genRand (Gen ary) = do
    s10 <- P.readPrimArray ary 0
    s11 <- P.readPrimArray ary 1
    s12 <- P.readPrimArray ary 2
    let p1 = a12f * s11 - a13nf * s10
        q1 = floorInt $ p1 / m1f
        r1 = p1 - fromIntegral q1 * m1f
        t1 = if r1 < 0.0 then r1 + m1f else r1
    P.writePrimArray ary 0 s11
    P.writePrimArray ary 1 s12
    P.writePrimArray ary 2 t1
    s20 <- P.readPrimArray ary 3
    s21 <- P.readPrimArray ary 4
    s22 <- P.readPrimArray ary 5
    let p2 = a21f * s22 - a23nf * s20
        q2 = floorInt $ p2 / m2f
        r2 = p2 - fromIntegral q2 * m2f
        t2 = if r2 < 0.0 then r2 + m2f else r2
        v = if t1 <= t2 then t1 - t2 + m1f else t1 - t2
    P.writePrimArray ary 3 s21
    P.writePrimArray ary 4 s22
    P.writePrimArray ary 5 t2
    return v
{-# INLINE mrg32k3a_genRand #-}

uniformDouble01M :: (PrimMonad m) => Gen (PrimState m) -> m Double
uniformDouble01M g = (* norm) <$> mrg32k3a_genRand g
{-# INLINE uniformDouble01M #-}

ub :: Word64
ub = m1sq - r
  where !m1sq = m1 * m1
        !r = m1sq `mod` 4294967296
{-# INLINE ub #-}

uniformW32 :: (PrimMonad m) => Gen (PrimState m) -> m Word32
uniformW32 gen = go
  where go = do
          v0 <- floor <$> mrg32k3a_genRand gen
          v1 <- floor <$> mrg32k3a_genRand gen
          let x = v0 * m1 + v1
          if x >= ub then go else return (fromIntegral (x .&. 4294967295))
{-# INLINE uniformW32 #-}

initialize :: (PrimMonad m, Integral a) => a -> m (Gen (PrimState m))
initialize seed = do
    let s' = fromIntegral seed
        s1 = fromIntegral $ s' `mod` m1
        s2 = fromIntegral $ s' `mod` m2
    ary <- P.newPrimArray 6
    P.writePrimArray ary 0 s1
    P.writePrimArray ary 1 s1
    P.writePrimArray ary 2 s1
    P.writePrimArray ary 3 s2
    P.writePrimArray ary 4 s2
    P.writePrimArray ary 5 s2
    return $ Gen ary
{-# INLINE initialize #-}

newtype Seed = Seed { fromSeed :: (Word32,Word32,Word32,Word32,Word32,Word32) }
  deriving (Eq, Show, Typeable)

save :: (PrimMonad m) => Gen (PrimState m) -> m Seed
save (Gen ary) = do
    t1 <- floor `liftM` P.readPrimArray ary 0
    t2 <- floor `liftM` P.readPrimArray ary 1
    t3 <- floor `liftM` P.readPrimArray ary 2
    t4 <- floor `liftM` P.readPrimArray ary 3
    t5 <- floor `liftM` P.readPrimArray ary 4
    t6 <- floor `liftM` P.readPrimArray ary 5
    return $ Seed (t1,t2,t3,t4,t5,t6)
{-# INLINE save #-}

restore :: (PrimMonad m) => Seed -> m (Gen (PrimState m))
restore (Seed (t1,t2,t3,t4,t5,t6)) = do
    let m1' = fromIntegral m1
        m2' = fromIntegral m2
    ary <- P.newPrimArray 6
    P.writePrimArray ary 0 $ fromIntegral $ t1 `mod` m1'
    P.writePrimArray ary 1 $ fromIntegral $ t2 `mod` m1'
    P.writePrimArray ary 2 $ fromIntegral $ t3 `mod` m1'
    P.writePrimArray ary 3 $ fromIntegral $ t4 `mod` m2'
    P.writePrimArray ary 4 $ fromIntegral $ t5 `mod` m2'
    P.writePrimArray ary 5 $ fromIntegral $ t6 `mod` m2'
    return $ Gen ary

-- EOF
