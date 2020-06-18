{- -*- mode: haskell; coding: utf-8-unix -*-  -}

module System.Random.MRG.Internal
  (
    StateVector
  , JumpMatrix

  , vecTrOn
  , matMulOn
  , matSqrOn
  ) where

data StateVector = SV (Integer,Integer,Integer)
data JumpMatrix  = JM (Integer,Integer,Integer) (Integer,Integer,Integer) (Integer,Integer,Integer)

mulOn :: Integer -> Integer -> Integer -> Integer
mulOn m x y = (x * y) `mod` m

vecTrOn :: Integer -> JumpMatrix -> StateVector -> StateVector
vecTrOn m (JM (x11,x12,x13) (x21,x22,x23) (x31,x32,x33)) (SV (y1,y2,y3))
  = SV (z1,z2,z3)
  where u = mulOn m
        z1 = ((x11 `u` y1) + (x12 `u` y2) + (x13 `u` y3)) `mod` m
        z2 = ((x21 `u` y1) + (x22 `u` y2) + (x23 `u` y3)) `mod` m
        z3 = ((x31 `u` y1) + (x32 `u` y2) + (x33 `u` y3)) `mod` m

matMulOn :: Integer -> JumpMatrix -> JumpMatrix -> JumpMatrix
matMulOn m xx (JM (y11,y12,y13) (y21,y22,y23) (y31,y32,y33))
  = JM (z11,z12,z13) (z21,z22,z23) (z31,z32,z33)
  where u = vecTrOn m
        SV (z11,z21,z31) = xx `u` (SV (y11,y21,y31))
        SV (z12,z22,z32) = xx `u` (SV (y12,y22,y32))
        SV (z13,z23,z33) = xx `u` (SV (y13,y23,y33))

matSqrOn :: Integer -> JumpMatrix -> JumpMatrix
matSqrOn m xx = matMulOn m xx xx

-- EOF
