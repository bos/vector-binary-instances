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

naiveGet' :: (Binary a, U.Unbox a) => Get (U.Vector a)
naiveGet' = do
    n <- get
    U.replicateM n get
-- A feeble attempt at simulating what will happen if we end up with a situation
-- where we are unable to specialize to the element type
{-# NOINLINE naiveGet' #-}

type V = BS.ByteString -> U.Vector Int

benchGetSize :: String -> BS.ByteString -> Benchmark
benchGetSize name bs = bgroup name
    [ bench "U.Vector Int"                 $ nf (decode :: V) bs
    , bench "naive U.Vector Int"           $ nf (runGet naiveGet :: V) bs
    , bench "noinline naive U.Vector Int"  $ nf (runGet naiveGet' :: V) bs
    ]

main = defaultMain
  [ bgroup "encode"
    [ bench "U.Vector Int 3"      $ nf encode vec1
    , bench "U.Vector Int 30"     $ nf encode vec2
    , bench "U.Vector Int 300"    $ nf encode vec3
    , bench "U.Vector Int 30000"  $ nf encode vec4
    , bench "U.Vector Int 300000" $ nf encode vec5
    ]
  , bgroup "decode"
    [ benchGetSize "size=3" bs1
    , benchGetSize "size=30" bs2
    , benchGetSize "size=300" bs3
    , benchGetSize "size=30000" bs4
    , benchGetSize "size=300000" bs5
    ]
  ]

