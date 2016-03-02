import Criterion.Main
import Data.Binary
import Data.Binary.Get

import qualified Data.ByteString.Lazy     as BS
import qualified Data.Vector.Unboxed as U
import Data.Vector.Binary


vec1,vec2,vec3,vec4,vec5 :: U.Vector Int
vec1 = U.enumFromN 0 3
vec2 = U.enumFromN 0 30
vec3 = U.enumFromN 0 300
vec4 = U.enumFromN 0 30000
vec5 = U.enumFromN 0 300000

bs1,bs2,bs3,bs4,bs5 :: BS.ByteString
bs1 = encode vec1
bs2 = encode vec2
bs3 = encode vec3
bs4 = encode vec4
bs5 = encode vec5

naiveGet :: (Binary a, U.Unbox a) => Get (U.Vector a)
naiveGet = do
    n <- get
    U.replicateM n get

type V = BS.ByteString -> U.Vector Int

main = defaultMain
  [ bgroup "encode"
    [ bench "U.Vector Int 3"      $ nf encode vec1
    , bench "U.Vector Int 30"     $ nf encode vec2
    , bench "U.Vector Int 300"    $ nf encode vec3
    , bench "U.Vector Int 30000"  $ nf encode vec4
    , bench "U.Vector Int 300000" $ nf encode vec5
    ]
  , bgroup "decode"
    [ bench "U.Vector Int 3"      $ nf (decode :: V) bs1
    , bench "U.Vector Int 30"     $ nf (decode :: V) bs2
    , bench "U.Vector Int 300"    $ nf (decode :: V) bs3
    , bench "U.Vector Int 30000"  $ nf (decode :: V) bs4
    , bench "U.Vector Int 300000" $ nf (decode :: V) bs5

    , bench "naive U.Vector Int 3"      $ nf (runGet naiveGet :: V) bs1
    , bench "naive U.Vector Int 30"     $ nf (runGet naiveGet :: V) bs2
    , bench "naive U.Vector Int 300"    $ nf (runGet naiveGet :: V) bs3
    , bench "naive U.Vector Int 30000"  $ nf (runGet naiveGet :: V) bs4
    , bench "naive U.Vector Int 300000" $ nf (runGet naiveGet :: V) bs5
    ]
  ]

