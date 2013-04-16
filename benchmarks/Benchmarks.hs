import Criterion.Main
import Data.Binary

import qualified Data.ByteString.Lazy     as BS
import qualified Data.Vector.Unboxed as U
import Data.Vector.Binary


vec1,vec2,vec3 :: U.Vector Int
vec1 = U.enumFromN 0 3
vec2 = U.enumFromN 0 30
vec3 = U.enumFromN 0 300

bs1,bs2,bs3 :: BS.ByteString
bs1 = encode vec1
bs2 = encode vec2
bs3 = encode vec3

type V = BS.ByteString -> U.Vector Int

main = defaultMain
  [ bgroup "encode"
    [ bench "U.Vector Int 3"   $ nf encode vec1
    , bench "U.Vector Int 30"  $ nf encode vec2
    , bench "U.Vector Int 300" $ nf encode vec3
    ]
  , bgroup "decode"
    [ bench "U.Vector Int 3"   $ nf (decode :: V) bs1
    , bench "U.Vector Int 30"  $ nf (decode :: V) bs2
    , bench "U.Vector Int 300" $ nf (decode :: V) bs3
    ]
  ]

