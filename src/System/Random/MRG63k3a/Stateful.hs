{- -*- mode: haskell; coding: utf-8-unix -*-  -}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies #-}

module System.Random.MRG63k3a.Stateful
    (
      Gen
    , initialize
    , uniform01M

    , GenIO
    , GenST

    , Seed
    , fromSeed
    ) where

import Control.Monad.Primitive (PrimMonad, PrimState, unsafeSTToPrim)
import Control.Monad.ST        (ST)
import qualified Data.Primitive.PrimArray as P
import Data.Typeable           (Typeable)
import Data.Int                (Int64)
import Data.Word
import Data.Bits               ((.&.),(.|.),shiftL)

import System.Random.MRG63k3a.Internal

import System.Random.Stateful

newtype Gen s = Gen (P.MutablePrimArray s Int64)

type GenIO = Gen (PrimState IO)

type GenST s = Gen (PrimState (ST s))

newtype T6 a = T6 { fromT6 :: (a,a,a,a,a,a) }

instance Functor T6 where
  fmap f (T6 (x1,x2,x3,x4,x5,x6)) = T6 (f x1, f x2, f x3, f x4, f x5, f x6)

instance (s ~ PrimState m, PrimMonad m) => StatefulGen (Gen s) m where
  uniformWord32 = uniformW32
  uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n go)
    where go = do
            x0 <- uniformW32 g
            x1 <- uniformW32 g
            pure $ (fromIntegral x0 `shiftL` 32) .|. fromIntegral x1
  {-# INLINE uniformWord32 #-}
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

uniform01M :: (PrimMonad m) => Gen (PrimState m) -> m Double
uniform01M g = (* norm) <$> fromIntegral <$> mrg63k3a_genRand g
{-# INLINE uniform01M #-}

ub :: Int64
ub = m1 - r
  where !r = m1 `mod` 4294967296
{-# INLINE ub #-}

uniformW32 :: (PrimMonad m) => Gen (PrimState m) -> m Word32
uniformW32 gen = go
  where go = do
          v <- mrg63k3a_genRand gen
          if v >= ub then go else return (fromIntegral (v .&. 4294967295))
{-# INLINE uniformW32 #-}

initialize :: (PrimMonad m, Integral a) => a -> m (Gen (PrimState m))
initialize seed = do
    let s' = fromIntegral seed
        s1 = fromIntegral $ s' `mod` m1
        s2 = fromIntegral $ s' `mod` m2
    ary <- P.newPrimArray 6
    writeState ary (s1,s1,s1,s2,s2,s2)
    return $ Gen ary
{-# INLINE initialize #-}

newtype Seed = Seed { fromSeed :: (Word64,Word64,Word64,Word64,Word64,Word64)}
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

-- EOF
