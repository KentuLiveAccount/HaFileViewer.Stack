Internal Sparse Index
======================

This module provides a pure data structure for sparse line indexing.
It maintains a map from line numbers to byte offsets for efficient
seeking, storing only every Kth line (default K=1024).

This is an internal implementation detail used by LineCache.

> module HaFileViewer.Internal.SparseIndex
>   ( -- * Types
>     SparseIndex
>   , IndexStep
>     
>     -- * Construction
>   , empty
>   , singleton
>   , fromList
>     
>     -- * Query
>   , lookup
>   , lookupNearest
>   , member
>   , size
>     
>     -- * Modification
>   , insert
>   , insertBatch
>   , delete
>   , clear
>     
>     -- * Utilities
>   , toList
>   , keys
>   , elems
>   ) where

> import Prelude hiding (lookup)
> import qualified Data.Map.Strict as Map
> import HaFileViewer.LineMap.Common (Offset)

Type Definitions
----------------

The sparse index is simply a map from line numbers to byte offsets.
We store every Kth line to balance memory usage vs. seek performance.

> type SparseIndex = Map.Map Integer Offset
> type IndexStep = Int

Construction
------------

> -- | Create an empty sparse index
> empty :: SparseIndex
> empty = Map.empty

> -- | Create a sparse index with a single entry
> singleton :: Integer -> Offset -> SparseIndex
> singleton = Map.singleton

> -- | Build sparse index from a list of (lineNum, offset) pairs
> fromList :: [(Integer, Offset)] -> SparseIndex
> fromList = Map.fromList

Query Operations
----------------

> -- | Look up the exact offset for a line number
> -- Returns Nothing if line is not in the index
> lookup :: Integer -> SparseIndex -> Maybe Offset
> lookup = Map.lookup

> -- | Find the nearest indexed line at or before the target line
> -- Returns (lineNum, offset) of the nearest indexed line
> -- This is the key function for seeking: we find the closest known
> -- position and scan forward from there
> lookupNearest :: Integer -> SparseIndex -> Maybe (Integer, Offset)
> lookupNearest target idx = Map.lookupLE target idx

> -- | Check if a line number is in the index
> member :: Integer -> SparseIndex -> Bool
> member = Map.member

> -- | Number of entries in the index
> size :: SparseIndex -> Int
> size = Map.size

Modification Operations
-----------------------

> -- | Insert a single (lineNum, offset) entry
> insert :: Integer -> Offset -> SparseIndex -> SparseIndex
> insert = Map.insert

> -- | Insert multiple entries at once (more efficient than multiple inserts)
> insertBatch :: [(Integer, Offset)] -> SparseIndex -> SparseIndex
> insertBatch entries idx = Map.union (Map.fromList entries) idx

> -- | Remove a line from the index
> delete :: Integer -> SparseIndex -> SparseIndex
> delete = Map.delete

> -- | Clear all entries (return to empty index)
> clear :: SparseIndex -> SparseIndex
> clear = const Map.empty

Utility Operations
------------------

> -- | Convert to list of (lineNum, offset) pairs
> toList :: SparseIndex -> [(Integer, Offset)]
> toList = Map.toList

> -- | Get all indexed line numbers
> keys :: SparseIndex -> [Integer]
> keys = Map.keys

> -- | Get all offsets
> elems :: SparseIndex -> [Offset]
> elems = Map.elems
