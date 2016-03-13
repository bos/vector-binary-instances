{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Binary
import Data.Vector.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

roundTrip :: forall v a. (Eq (v a), Binary (v a), VG.Vector v a)
          => v a -> Property
roundTrip v =
    let v' = decode $ encode v :: v a
    in property $ v' == v

main = defaultMain $ testGroup "Vector Binary instances"
    [ testProperty "Unboxed"  $ roundTrip $ VU.enumFromTo z 100
    , testProperty "Storable" $ roundTrip $ VS.enumFromTo z 100
    , testProperty "Boxed"    $ roundTrip $ V.enumFromTo  z 100
    ]
  where
    z = 0 :: Int
