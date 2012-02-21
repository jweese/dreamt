> module Vocabulary
>        ( Vocabulary
>        , fromList
>        , size
>        , value
>        , word
>        )
>        where

> import qualified Data.Set as Set
> import qualified Data.Map as Map

This module implements a vocabulary. Originally, I was not planning to
implement a vocab, but the String type is not an instance of the typeclass
Ix, which means it can't be used as the indexing type of an Array. Instead,
we'll map Strings to Ints, which can be used as an index.

> data Vocabulary = V
>                 { words :: Map.Map Int String
>                 , ids :: Map.Map String Int
>                 }
> instance Show Vocabulary where
>     show (V _ is) = "vocabulary (" ++ (show $ Map.size is) ++ " entries)"

Looking up the ID of a word, or vice-versa, is just a map lookup.

> value :: Vocabulary -> String -> Maybe Int
> value (V _ is) w = Map.lookup w is

> word :: Vocabulary -> Int -> Maybe String
> word (V ws _) i = Map.lookup i ws

For the size, we know for each (V ws is), size ws == size is, so it doesn't
matter which one we return.

> size :: Vocabulary -> Int
> size (V _ is) = Map.size is

This is the only way to construct a vocabulary. We use Set.{fromList,toList}
as a slighly better way to find uniques. We could use Data.List.nub, but its
behavior is O(n^2), and this is O(n log(n))

> fromList :: [String] -> Vocabulary
> fromList xs = V ws is
>    where xs' = Set.toList $ Set.fromList xs
>          kvs = zip [0..] xs'
>          ws = Map.fromAscList $ kvs
>          is = Map.fromList $ map (\(k,v) -> (v,k)) kvs
