> module IBM.IndexTable
>     ( IndexTable
>     , fromList
>     , lookup
>     , (!)
>     , modelSize
>     , bounds
>     , foreignIndices
>     ) where

This is an attempt to write a faster lookup table, where we want to map all
the (Int,Int) pairs we've seen to a contiguous range of integers. 

Let's review why we want to do this: In Model1, the probabilities are going to
be stored in an array, which needs to be indexed by an instance of Ix. We want
to look up probabilities by an (Int,Int) pair. But if we used (Int,Int) as the
index type for these probabilities, we'd end up allocating way too much memory.

For example, suppose E is the English vocabulary and F is foreign. If |E| is
1000, and so is |F|, allocating an array with bounds (0,0) and (|E|,|F|) will
allocate 1,000,000 entries, even though most of those entries will never be
touched. It only gets worse with realistic vocabularies.

So obviously we need to map arbitrary (Int,Int) pairs to a contiguous set of
integers. 

> import Prelude hiding (lookup)
> import Data.List (foldl')
> import qualified Data.Set as Set
> import qualified Data.IntMap as IntMap

So we've decided to use a two-level trie, here modeled as two levels of maps.
Originally we just used the Haskell standard Map (Int,Int) Int -- but that
has some disadvantages:
1. We have to store all the IntPairs that we're using as keys, even though
the foreign word f of (f,e) may be re-used may times.
2. Lookup is O(logn) in the number of pairs.
The trie will provide us with both space and time savings.

> newtype IndexTable = T (IntMap.IntMap (IntMap.IntMap Int))
>                    deriving Show

> empty :: IndexTable
> empty = T IntMap.empty

So fromList is pretty simple; it's just a fold over all Int pairs that we've
seen. One thing to note is that we want all the pairs (f,e) with the same f
to be contiguous -- this means we have to sort all the pairs before we
accumulate them.

The fastest way to do that is the composition Set.toAscList . Set.fromList,
whose O(n log n) behavior is better than nub's O(n^2).

> fromList :: [(Int,Int)] -> IndexTable
> fromList = snd . foldl' insert (1,empty) . Set.toAscList . Set.fromList

The insert function is pretty ugly. There ought to be a way to refactor this
using the fact that Maybe is a monad (see, for instance, the lookup function
below). In the meantime, it's maybe slightly less efficient than necessary.
Note the use of seq to avoid the index i being built up as a thunk.

> insert :: (Int,IndexTable) -> (Int,Int) -> (Int,IndexTable)
> insert s@(i,t@(T m)) x@(f,e) = case lookup t x of
>     Just _ -> s
>     _ -> let em = case IntMap.lookup f m of
>                       Just m' -> IntMap.insert e i m'
>                       _ -> IntMap.singleton e i
>          in i `seq` (i+1, T $ IntMap.insert f em m)

Maybe is a monad, so we can sequence these two lookups easily!

> lookup :: IndexTable -> (Int,Int) -> Maybe Int
> lookup (T m) (f,e) = IntMap.lookup f m >>= \m' -> IntMap.lookup e m'

This is the more commonly used method; we unsafely pull the looked-up result
from its Maybe wrapper.

> (!) :: IndexTable -> (Int,Int) -> Int
> t ! x = case lookup t x of
>     Just n -> n
>     _ -> error $ "key " ++ (show x) ++ " not in IndexTable"

> size :: IndexTable -> Int
> size (T m) = sum $ map IntMap.size $ IntMap.elems m

> modelSize :: IndexTable -> (Int,Int)
> modelSize t = (1,size t)

Similarly, this is used for allocating the array of marginal sums.

> bounds :: IndexTable -> (Int,Int)
> bounds t@(T m) = (negate $ last ks, size t)
>     where ks = IntMap.keys m

> foreignIndices :: IndexTable -> [Int]
> foreignIndices (T m) = concatMap (\(k,v) -> replicate (IntMap.size v) (negate k)) (IntMap.assocs m)
 
