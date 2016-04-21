import Criterion.Main
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy     as BS
import qualified Data.Vector.Unboxed as U
import Data.Vector.Binary

-- We take care to avoid using the @Binary@ instances here to avoid issues with
-- overlapping instances as the install plan will involve two different versions
-- of k@vector-binary-instances@ (as @Criterion@ transitively depends upon
-- @vector-binary-instances@). See #7 for details.

vec1,vec2,vec3,vec4,vec5 :: U.Vector Int
vec1 = U.enumFromN 0 3
vec2 = U.enumFromN 0 30
vec3 = U.enumFromN 0 300
vec4 = U.enumFromN 0 30000
vec5 = U.enumFromN 0 300000

bs1,bs2,bs3,bs4,bs5 :: BS.ByteString
bs1 = encodeVector vec1
bs2 = encodeVector vec2
bs3 = encodeVector vec3
bs4 = encodeVector vec4
bs5 = encodeVector vec5

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

encodeVector :: U.Vector Int -> BS.ByteString
encodeVector = runPut . genericPutVector

benchGetSize :: String -> BS.ByteString -> Benchmark
benchGetSize name bs = bgroup name
    [ bench "U.Vector Int"                 $ nf (runGet genericGetVector :: V) bs
    , bench "naive U.Vector Int"           $ nf (runGet naiveGet         :: V) bs
    , bench "noinline naive U.Vector Int"  $ nf (runGet naiveGet'        :: V) bs
    ]

main = defaultMain
  [ bgroup "encode"
    [ bench "U.Vector Int 3"      $ nf encodeVector vec1
    , bench "U.Vector Int 30"     $ nf encodeVector vec2
    , bench "U.Vector Int 300"    $ nf encodeVector vec3
    , bench "U.Vector Int 30000"  $ nf encodeVector vec4
    , bench "U.Vector Int 300000" $ nf encodeVector vec5
    ]
  , bgroup "decode"
    [ benchGetSize "size=3" bs1
    , benchGetSize "size=30" bs2
    , benchGetSize "size=300" bs3
    , benchGetSize "size=30000" bs4
    , benchGetSize "size=300000" bs5
    ]
  ]

