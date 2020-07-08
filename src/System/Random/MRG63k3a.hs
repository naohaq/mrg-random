{- -*- mode: haskell; coding: utf-8-unix -*-  -}
{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, FlexibleContexts,
    ScopedTypeVariables, TypeFamilies #-}

module System.Random.MRG63k3a
    (
      Gen
    , initialize

    , uniform01

    , Seed
    , fromSeed
    , save
    , restore

    , jump
    ) where

import Data.Typeable (Typeable)
import Data.Int  (Int64)
import Data.Word (Word32,Word64)
import Data.Bits ((.&.))

import System.Random

import System.Random.MRG.Internal
import System.Random.MRG63k3a.Internal

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

initialize :: (Integral a) => a -> Gen
initialize seed = Gen (s1,s1,s1,s2,s2,s2)
  where s1 = fromIntegral seed `mod` m1
        s2 = fromIntegral seed `mod` m2
{-# INLINE initialize #-}

uniform01 :: Gen -> (Double, Gen)
uniform01 gen = (w,gen')
  where (v,gen') = mrg63k3a_genRand gen
        !w = norm * fromIntegral v
{-# INLINE uniform01 #-}

ub :: Int64
ub = m1 - r
  where !r = m1 `mod` 4294967296
{-# INLINE ub #-}

uniformW32 :: Gen -> (Word32, Gen)
uniformW32 gen = go gen
  where go g = if x >= ub then go g' else (fromIntegral (x .&. 4294967295), g')
          where (x,g') = mrg63k3a_genRand g
{-# INLINE uniformW32 #-}

newtype Seed = Seed { fromSeed :: (Word64,Word64,Word64,Word64,Word64,Word64) }
  deriving (Eq, Show, Typeable)

save :: Gen -> Seed
save (Gen (s10,s11,s12,s20,s21,s22)) = Seed (t1,t2,t3,t4,t5,t6)
  where t1 = fromIntegral s10
        t2 = fromIntegral s11
        t3 = fromIntegral s12
        t4 = fromIntegral s20
        t5 = fromIntegral s21
        t6 = fromIntegral s22
{-# INLINE save #-}

restore :: Seed -> Gen
restore (Seed (t1,t2,t3,t4,t5,t6)) = Gen (s10,s11,s12,s20,s21,s22)
  where m1' = fromIntegral m1
        m2' = fromIntegral m2
        s10 = fromIntegral $ t1 `mod` m1'
        s11 = fromIntegral $ t2 `mod` m1'
        s12 = fromIntegral $ t3 `mod` m1'
        s20 = fromIntegral $ t4 `mod` m2'
        s21 = fromIntegral $ t5 `mod` m2'
        s22 = fromIntegral $ t6 `mod` m2'
{-# INLINE restore #-}

jump :: Int -> Gen -> Gen
jump e g@(Gen (s10,s11,s12,s20,s21,s22))
  | e > 64 || e < 0 = error "Jump factor must be in the range of [0,64]"
  | e == 0          = g
  | otherwise       = Gen (t10,t11,t12,t20,t21,t22)
  where m1' = fromIntegral m1
        m2' = fromIntegral m2
        v1  = fromIntegral <$> SV (s10, s11, s12)
        v2  = fromIntegral <$> SV (s20, s21, s22)
        (b1,b2) = jmtxs !! (e-1)
        w1  = vecTrModW64 m1' b1 v1
        w2  = vecTrModW64 m2' b2 v2
        SV (t10,t11,t12) = fromIntegral <$> w1
        SV (t20,t21,t22) = fromIntegral <$> w2

-- EOF
