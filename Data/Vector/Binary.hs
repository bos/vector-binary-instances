{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Module    : Data.Vector.Binary
-- Copyright : (c) Don Stewart 2010-2012

-- License   : BSD3
--
-- Maintainer: Don Stewart <dons00@gmail.com>
-- Stability : provisional
-- Portability: GHC only

-- Instances for Binary for the types defined in the vector package,
-- making it easy to serialize vectors to and from disk. We use the
-- generic interface to vectors, so all vector types are supported.
--
-- All functions in this module use same data format. Different
-- representations for vector length and its elements could be used
-- but general shape is same.
--
-- > [number of elements]
-- > [vector element    ] : N times
--
-- To serialize a vector:
--
-- > *Data.Vector.Binary> let v = Data.Vector.fromList [1..10]
-- > *Data.Vector.Binary> v
-- > fromList [1,2,3,4,5,6,7,8,9,10] :: Data.Vector.Vector
-- > *Data.Vector.Binary> encode v
-- > Chunk "\NUL\NUL\NUL\NUL\NUL...\NUL\NUL\NUL\t\NUL\NUL\NUL\NUL\n" Empty
--
-- Which you can in turn compress before writing to disk:
--
-- > compress . encode $ v
-- > Chunk "\US\139\b\NUL\NUL\N...\229\240,\254:\NUL\NUL\NUL" Empty
--
--------------------------------------------------------------------
module Data.Vector.Binary (
    genericGetVector
  , genericGetVectorWith
  , genericPutVector
  , genericPutVectorWith
  ) where

import Data.Binary

import qualified Data.Vector.Generic   as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed   as U
import qualified Data.Vector.Storable  as S
import qualified Data.Vector.Primitive as P
import Data.Vector (Vector)
import System.IO.Unsafe

import Foreign.Storable (Storable)

-- Enumerate the instances to avoid the nasty overlapping instances.

-- | Boxed, generic vectors.
instance Binary a => Binary (Vector a) where
    put = genericPutVector
    get = genericGetVector
    {-# INLINE get #-}

-- | Unboxed vectors
instance (U.Unbox a, Binary a) => Binary (U.Vector a) where
    put = genericPutVector
    get = genericGetVector
    {-# INLINE get #-}

-- | Primitive vectors
instance (P.Prim a, Binary a) => Binary (P.Vector a) where
    put = genericPutVector
    get = genericGetVector
    {-# INLINE get #-}

-- | Storable vectors
instance (Storable a, Binary a) => Binary (S.Vector a) where
    put = genericPutVector
    get = genericGetVector
    {-# INLINE get #-}

------------------------------------------------------------------------

-- | Deserialize vector using custom parsers.
genericGetVectorWith :: G.Vector v a
    => Get Int       -- ^ Parser for vector size
    -> Get a         -- ^ Parser for vector's element
    -> Get (v a)
{-# INLINE genericGetVectorWith #-}
genericGetVectorWith getN getA = do
    n <- getN
    v <- return $ unsafePerformIO $ GM.unsafeNew n
    let go 0 = return ()
        go i = do x <- getA
                  () <- return $ unsafePerformIO $ GM.unsafeWrite v (n-i) x
                  go (i-1)
    () <- go n
    return $ unsafePerformIO $ G.unsafeFreeze v

-- | Generic put for anything in the G.Vector class which uses custom
--   encoders.
genericPutVectorWith :: G.Vector v a
    => (Int -> Put)  -- ^ Encoder for vector size
    -> (a   -> Put)  -- ^ Encoder for vector's element
    -> v a -> Put
{-# INLINE genericPutVectorWith #-}
genericPutVectorWith putN putA v = do
    putN (G.length v)
    G.mapM_ putA v

-- | Generic function for vector deserialization.
genericGetVector :: (G.Vector v a, Binary a) => Get (v a)
{-# INLINE genericGetVector #-}
genericGetVector = genericGetVectorWith get get

-- | Generic put for anything in the G.Vector class.
genericPutVector :: (G.Vector v a, Binary a) => v a -> Put
{-# INLINE genericPutVector #-}
genericPutVector = genericPutVectorWith put put
