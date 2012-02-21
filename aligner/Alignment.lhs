> module Alignment (Alignment, empty, addLink, Alignment.intersect, fromString) where

The imports go at the top; we will need this for output.

> import Data.List (foldl',intercalate,intersect)

This module represent an Alignment object. We're going to use a Map, that maps
foreign indices to lists of English indices.

> import qualified Data.IntMap as IntMap
> newtype Alignment = A (IntMap.IntMap [Int])

The first function we need to export is the empty Alignment (an empty map):

> empty :: Alignment
> empty = A $ IntMap.empty

The other function that we need is to add a link. It will take in the foreign
and English indices. It checks to see if the foreign index is already in the
underlying map. If it is, we cons the English index to its list. Otherwise,
we insert a singleton list.

One intricacy is that we have to check the list to see if the English index
is already present, too.

> addLink :: Alignment -> Int -> Int -> Alignment
> addLink a@(A map) f e = case IntMap.lookup f map of
>     Nothing -> A $ IntMap.insert f [e] map
>     Just es -> if e `elem` es then a else A $ IntMap.insertWith (++) f [e] map

It will occasionally be useful to intersect two alignments:

> intersect :: Alignment -> Alignment -> Alignment
> intersect (A a) (A b) = A $ IntMap.intersectionWith (Data.List.intersect) a b

One last thing: we should make Alignment an instance of Show, so that it will
get printed nicely for the output:

> instance Show Alignment where
>     show (A map) = IntMap.foldWithKey g "" map
>         where g f es s = showLinks f es ++ " " ++ s

The show function is written it terms of showLinks, which will produce a 
String from a particular key-value pair of the map. For instance, if the map
contained the mapping 0 -> [2,3,4], showLinks will produce the String
"0-2 0-3 0-4".

> showLinks :: Int -> [Int] -> String
> showLinks _ [] = ""
> showLinks f es = let showOneLink x y = (show x) ++ "-" ++ (show y)
>     in intercalate " " $ map (showOneLink f) es

Finally, we want a function for reading an alignment from a String. We can
define it as a fold using addLink. We include a parameter to show whether
the alignment string should be reversed when read -- that is, turned from a
source-target alignment into a target-source one.

> fromString :: Bool -> String -> Alignment
> fromString rev = foldl' (\a -> uncurry $ addLink a) empty . map (readLink rev) . words

> readLink :: Bool -> String -> (Int,Int)
> readLink rev s = if not rev then (a,b) else (b,a)
>     where (x,y) = break (=='-') s
>           a = read x
>           b = read $ tail y
