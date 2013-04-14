{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Module    : Data.Vector.Cereal
-- Copyright : (c) Don Stewart 2010-2012

-- License   : BSD3
--
-- Maintainer: Don Stewart <dons00@gmail.com>
-- Stability : provisional
-- Portability: GHC only
--
-- Instances for Serialize for the types defined in the vector package,
-- making it easy to serialize vectors to and from disk. We use the
-- generic interface to vectors, so all vector types are supported.
--
--------------------------------------------------------------------

module Data.Vector.Cereal () where

import Data.Serialize
import Control.Monad

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import Data.Vector (Vector)

import System.IO.Unsafe
import qualified Data.Vector.Generic.Mutable as M
import Foreign.Storable (Storable)

-- | Boxed, generic vectors.
instance Serialize a => Serialize (Vector a) where
    put = putGeneric
    get = getGeneric
    {-# INLINE get #-}

-- | Unboxed vectors
instance (U.Unbox a, Serialize a) => Serialize (U.Vector a) where
    put = putGeneric
    get = getGeneric
    {-# INLINE get #-}

-- | Storable vectors
instance (Storable a, Serialize a) => Serialize (S.Vector a) where
    put = putGeneric
    get = getGeneric
    {-# INLINE get #-}

------------------------------------------------------------------------
    
-- this is morally sound, if very awkward.
-- all effects are contained, and can't escape the unsafeFreeze
getGeneric :: (G.Vector v a, Serialize a) => Get (v a)
{-# INLINE getGeneric #-}
getGeneric = do
    n  <- get

    -- new unitinialized array
    mv <- lift $ M.new n

    let fill i
            | i < n = do
                x <- get
                (unsafePerformIO $ M.unsafeWrite mv i x) `seq` return ()
                fill (i+1)

            | otherwise = return ()

    fill 0

    lift $ G.unsafeFreeze mv
    
-- | Generic put for anything in the G.Vector class.
putGeneric :: (G.Vector v a, Serialize a) => v a -> Put
{-# INLINE putGeneric #-}
putGeneric v = do
    put (G.length v)
    mapM_ put (G.toList v)

lift :: IO b -> Get b
lift = return .unsafePerformIO
