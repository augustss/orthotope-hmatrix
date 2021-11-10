{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Array.ShapedS.MatMul(matMul) where
import Prelude hiding ((<>))
import GHC.Stack
import GHC.TypeLits
import Data.Array.Internal(valueOf)
import Data.Array.ShapedS
import Numeric.LinearAlgebra as N

matMul :: forall m n o a .
          (HasCallStack, N.Numeric a, KnownNat m, KnownNat n, KnownNat o) =>
          Array [m, n] a -> Array [n, o] a -> Array [m, o] a
matMul x y =
  let n = valueOf @n
      o = valueOf @o
      x' = N.reshape n $ toVector x
      y' = N.reshape o $ toVector y
      xy' = x' N.<> y'
      xy = fromVector $ N.flatten xy'
  in  xy
