{-# LANGUAGE DataKinds #-}
module Data.Array.RankedS.MatMul(matMul) where
import Prelude hiding ((<>))
import GHC.Stack
import Data.Array.RankedS
import Numeric.LinearAlgebra as N

matMul :: (HasCallStack, N.Numeric a) => Array 2 a -> Array 2 a -> Array 2 a
matMul x y =
  case (shapeL x, shapeL y) of
    ([m, n], [n', o]) | n == n' ->
      let x' = N.reshape n $ toVector x
          y' = N.reshape o $ toVector y
          xy' = x' N.<> y'
          xy = fromVector [m, o] $ N.flatten xy'
      in  xy
    sz -> error $ "matMul: expected two conforming two-dimensional arrays, got shape " ++ show sz
