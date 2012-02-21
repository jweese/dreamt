> module Align where

> import qualified Data.Map as Map
> import qualified Data.Set as Set
> import qualified Vocabulary as Voc
> import Alignment
> import IBM.Model1
> import qualified IBM.IndexTable as IT
> import Data.List (foldl')
> import Data.Maybe (mapMaybe)
> import Control.Applicative ((<$>))
> import System.Console.GetOpt
> import System.Environment

This is the main alignment module. It countains functions for training a model
and producing outputs.

It's a simple fold to run many iterations of training over the same data.

> train :: Int -> Model1 -> [([Int],[Int])] -> Model1
> train k m ss = foldl' reEstimate m $ replicate k ss

Now let's talk for a moment about input. Assuming the file is read as one
big string (a reasonable assumption, given readFile's behavior), we can turn
it into the form of sentences that we're interested using the following:

> sentences :: String -> [[String]]
> sentences = (map words) . lines

> vocab :: [[String]] -> Voc.Vocabulary
> vocab = Voc.fromList . concat

> toIds :: Voc.Vocabulary -> [[String]] -> [[Int]]
> toIds v = map (mapMaybe $ Voc.value v)

Now, a function that takes a corpus, and returns a list of all (Int,Int)
collocations that appear in the corpus. We prepend a 0 to the front of each
foreign sentence because 0 is the ID of the null token. This list will be
used to create an IndexTable.

> collocations :: [([Int],[Int])] -> [(Int,Int)]
> collocations = concatMap (\(fs,es) -> [(f,e) | f <- 0:fs, e <- es])

And, the first look at the outside world for the day! This small function
reads the input from a file and pipes it through our sentences function.

> readSentences :: Maybe Int -> String -> IO [[String]]
> readSentences (Just n) f = take n <$> sentences <$> readFile f
> readSentences _ f = sentences <$> readFile f

We're getting ready to write the main function, so let's first deal with
command-line arguments. These are a direct port from Adam's initial
implementation.

And a datatype for holding the runtime settings.

> data Settings = Settings
>     { dataPrefix :: String
>     , englishSuffix :: String
>     , foreignSuffix :: String
>     , numSentences :: Maybe Int
>     , numIterations :: Int
>     }
>     deriving (Eq,Show)

And their defaults:

> defaultSettings :: Settings
> defaultSettings = Settings
>     { dataPrefix = "data/hansards"
>     , englishSuffix = "e"
>     , foreignSuffix = "f"
>     , numSentences = Nothing
>     , numIterations = 5
>     }

Now, a description of the options themselves. The interesting thing here is
that when options are parsed, they will return a list of functions of type
Settings -> Settings. Each function replaces a record in settings with a new
value. We'll fold these functions over the default settings in parseArgs.

> options :: [OptDescr (Settings -> Settings)]
> options =
>     [ Option ['d'] ["data"] (ReqArg (\x s -> s { dataPrefix = x }) "<filename>") "Data filename prefix"
>     , Option ['e'] ["english"] (ReqArg (\x s -> s {englishSuffix = x }) "en") "Suffix of English filename"
>     , Option ['f'] ["french"] (ReqArg (\x s -> s {foreignSuffix = x }) "fr") "Suffix of French filename"
>     , Option ['n'] ["num_sentences"] (ReqArg (\x s -> s { numSentences = Just $ read x }) "N") "Number of sentences to use for training and alignment"
>     , Option ['t'] ["train_iters"] (ReqArg (\x s -> s {numIterations = read x }) "N") "Number of iterations of E-M"
>     ]

> parseArgs :: IO Settings
> parseArgs = do
>     argv <- getArgs
>     case getOpt Permute options argv of
>         (o,_,[]) -> return $ foldr ($) defaultSettings o
>         (_,_,es) -> ioError $ userError $ concat es ++ usageInfo header options
>     where header = "usage: align [OPTION...]"

Now we can define some convenience functions to get things out of the Settings.

> englishFile :: Settings -> String
> englishFile s = dataPrefix s ++ "." ++ englishSuffix s
> foreignFile :: Settings -> String
> foreignFile s = dataPrefix s ++ "." ++ foreignSuffix s

With the option parsing completed, we can finally write the main function:

> main :: IO ()
> main = do
>     settings <- parseArgs
>     en <- readSentences (numSentences settings) (englishFile settings)
>     fr <- readSentences (numSentences settings) (foreignFile settings)
>     let ev = vocab en
>         fv = vocab (["#NULL#"]:fr)
>         c = zip (toIds fv fr) (toIds ev en)
>         it = IT.fromList $ collocations c
>         m = uniform it $ Voc.size ev
>         m' = train (numIterations settings) m c
>     mapM_ print $ map (uncurry (viterbiAlignment m')) c

