{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
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

module Data.Vector.Binary () where

import Data.Binary

import qualified Data.Vector.Generic   as G
import qualified Data.Vector.Unboxed   as U
import qualified Data.Vector.Storable  as S
import qualified Data.Vector.Primitive as P
import Data.Vector (Vector)

import Foreign.Storable (Storable)

-- Enumerate the instances to avoid the nasty overlapping instances.

-- | Boxed, generic vectors.
instance Binary a => Binary (Vector a) where
    put = putGeneric
    get = getGeneric
    {-# INLINE get #-}

-- | Unboxed vectors
instance (U.Unbox a, Binary a) => Binary (U.Vector a) where
    put = putGeneric
    get = getGeneric
    {-# INLINE get #-}

-- | Primitive vectors
instance (P.Prim a, Binary a) => Binary (P.Vector a) where
    put = putGeneric
    get = getGeneric
    {-# INLINE get #-}

-- | Storable vectors
instance (Storable a, Binary a) => Binary (S.Vector a) where
    put = putGeneric
    get = getGeneric
    {-# INLINE get #-}

------------------------------------------------------------------------

getGeneric :: (G.Vector v a, Binary a) => Get (v a)
{-# INLINE getGeneric #-}
getGeneric = do
    n  <- get
    G.replicateM n get

-- | Generic put for anything in the G.Vector class.
putGeneric :: (G.Vector v a, Binary a) => v a -> Put
{-# INLINE putGeneric #-}
putGeneric v = do
    put (G.length v)
    G.mapM_ put v
