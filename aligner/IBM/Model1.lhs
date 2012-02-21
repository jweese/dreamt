> module IBM.Model1 (Model1,
>                    uniform, 
>                    reEstimate,
>                    viterbiAlignment) where

> import Alignment
> import Data.List (maximum, foldl')
> import qualified Data.Map as Map
> import qualified Data.Set as Set
> import qualified Data.Array.Unboxed as UArray
> import qualified IBM.IndexTable as IT


This module defines IBM Model 1. Model 1 is a statistical word-based 
translation model, but we usually use it as an alignment model. The model
defines a probability

P(e,a|f)

That is, the probability of an English sentence and an alignment, given some
foreign sentence. Once we have this probability, we can calculate the
translation probability of some sentence pair by marginalizing over all
possible alignments.

p(e|f) = sum_a { P(e,a|f) }

Model 1 depends only on the lexical translation probabilities of the words
of each sentence:

(*) P(e,a|f) = (1/Z) prod_j { p(e_j|f_(a_j)) }

Z is a normalizing constant, and f_(a_j) means the foreign word that is aligned
to e_j under the alignment a.

The parameters of the model are therefore these lexical translation
probabilities p(e|f). Storing these is enough to define an entire model:

Here's the definition of a Model 1 object itself. It may either be
an array holding a probability for all of the pairs we've seen (the M
constructor), or, for initializing E-M, it may be a uniform distribution
(using the U constructor; see the function uniform).

> data Model1 = M IT.IndexTable (UArray.UArray Int Double)
>             | U IT.IndexTable Double

Note that the array used by the M constructor is one-dimensional. We need some
way of mapping pairs (Int,Int) to Ints that we can use for the array indices.
Why don't we just use (Int,Int) as the index? (Int,Int) is an instance of Ix,
the array index class, after all. The problem is that bounding the array by
an Int pair produces a large two-dimensional array. Since most English words
haven't been seen with most foreign words, that's a lof of empty space.

The (Int,Int) to Int mapping is called an IndexTable (see its module).

> indexTable :: Model1 -> IT.IndexTable
> indexTable (M ct _) = ct
> indexTable (U ct _) = ct

Querying the model for a probability:

> probability :: Model1 -> (Int,Int) -> Double
> probability (M t ps) x = (UArray.!) ps $ t IT.! x
> probability (U _ d) _ = d

Note that the uniform distribution always returns the same result.

However, in general, we're interested in using this as an alignment model. To
do this, we will assign some sentence pair the Viterbi alignment, namely

a' = argmax_a { P(e,a|f) }

The following derivation appears in Och and Ney, "Comparison of Statistical
Alignment Models" for Model 2:

a' = argmax_a { P(e,a|f) }
   = argmax_a { p(L_e|L_f) * prod_j { p(a_j|j,L_f)p(e_j|f_(a_j)) } }
   = argmax_a { prod_j { p(a_j|j,L_f)p(e_j|f_(a_j)) } }

In Model 1, since in fact, the alignment probability does not matter, we just
have a product of lexical translation probabilities:

   = argmax_a { prod_j { p(e_j|f_(a_j)) } }

Each of the j English words can be optimized independently. We start by 
defining a function that, given an English word, a list of foreign words,
and a model, will return the index of the best foreign word to align to.

However, if the best index is the unaligned token 0, we'll return Nothing.

> bestIndex :: Model1 -> [Int] -> Int -> Maybe Int
> bestIndex m fs e = if j == 0 then Nothing else Just $ j - 1
>     where j = snd $ maximum $ zip [probability m (f,e) | f <- 0:fs] [0..]

Using bestIndex, we can write a function that, given a model and foreign
sentence, can take an Alignment, English word and English index, and add the
best link to that Alignment.

> addBestLink :: Model1 -> [Int] -> Alignment -> (Int,Int) -> Alignment
> addBestLink m fs a (e,i) = case bestIndex m fs e of
>     Just j -> addLink a j i
>     Nothing -> a

Now we will want to write a function that takes a sentence pair and
returns its alignment:

> viterbiAlignment :: Model1 -> [Int] -> [Int] -> Alignment
> viterbiAlignment m fs es = 
>     foldl' (addBestLink m fs) Alignment.empty $ zip es [0..]

A sentence is represented by a list of Ints for the tokens of the sentence.
The Alignment is calculated by using a fold: starting with the empty alignment,
for each English word in es, we use addBestLink to add one link. The resulting
alignment is used to add another link, and so on.

Now, let us turn to the question of training the model. Without any data, we
will want to start with a uniform distribution: for each e and f, we want
p(e|f) to always produce the same value. In order for it to be a proper
probability distribution, the value should be 1 / |V|, where V is the size of
the English vocabulary. The following function takes the English vocabulary size
as a parameter and returns the appropriate distribution:

> uniform :: IT.IndexTable -> Int -> Model1
> uniform ct v = U ct $ 1 / (fromIntegral v)

Now to do E-M on Model 1 (or on any model, I guess) there are two steps. In the
E step, we calculate the expected counts of parameters under the model. In the
M step, we use those counts to re-estimate the model parameters. Thus, we can
write a high-level description of E-M as the reEstimate function.

> reEstimate :: Model1 -> [([Int],[Int])] -> Model1
> reEstimate m c = fromCounts (indexTable m) $ concatMap (expectedCounts m) c

Now, to calculate the expected count C(e,f) we can use the following formula:

C(e,f) = p(e|f) / sum_f' {p e | f' }

> expectedCounts :: Model1 -> ([Int],[Int]) -> [((Int,Int),Double)]
> expectedCounts m (fs,es) = concatMap (singleExpectedCount m $ 0:fs) es

> singleExpectedCount :: Model1 -> [Int] -> Int -> [((Int,Int),Double)]
> singleExpectedCount m fs e = 
>     let t = sum [probability m (f,e) | f <- fs]
>     in t `seq` [((f,e),probability m (f,e) / t) | f <- fs]

Thus, the result of the concatMap in reEstimate above will produce a list of
integer pairs along with their expected counts. We will
have to sum over this list and re-normalize the get the new results.

That's where fromCounts comes in. The first thing that it does is change the
list of counts: for each pair of (e,f) and probability coming in, it uses the
index table to map (e,f) to its index i. It also duplicates the probability
p, associating it with the index (-f). In this way, we can sum the expected
counts and calculate the marginal values

Z(f) = sum_e { C(e|f) }

all in one traversal of the list! Before, we created two separate lists and
two arrays by mapping the count list cs to two different forms. This turns out
to have undesirable memory behavior: the two arrays aren't constructed at the
same time, and they both depend on having all of cs. Thus, the entire list
has to be constructed and stored in memory.

With one array, there's no extra storage! expectedCounts generates the counts,
and each generated pair is immediately consumed by fromCounts. We never need
to store the entire list.

Note also the use of seq to force evaluation of the array -- we don't want to
build up a huge thunk.

> fromCounts :: IT.IndexTable -> [((Int,Int),Double)] -> Model1
> fromCounts t cs = arr `seq` M t arr
>     where cs' = concatMap (addMarginalCount t) cs
>           addMarginalCount t (x@(f,_),p) = [(t IT.! x,p),(negate f,p)]
>           tmp = UArray.accumArray (+) 0 (IT.bounds t) cs'
>           arr = normalize t tmp

In the normalization function, we generate a list of divisors by pulling them
out of the array in the right order (according to foreignIndices). We
divide each accumulated count by its normalizer Z(f) to get the probability,
which we store in an array.

> type CountArray = UArray.UArray Int Double
> normalize :: IT.IndexTable -> CountArray -> CountArray
> normalize t arr = 
>     let zs = map (arr UArray.!) (IT.foreignIndices t)
>         es = map snd $ dropWhile (\x -> fst x < 1) (UArray.assocs arr)
>         ps = zipWith (/) es zs
>     in UArray.listArray (IT.modelSize t) ps

