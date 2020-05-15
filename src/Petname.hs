{-# LANGUAGE TemplateHaskell #-}

module Petname
  ( random,
  )
where

import qualified Data.ByteString
import Data.FileEmbed
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Protolude
import System.FilePath (FilePath, takeFileName)
import System.Random (randomRIO)

random :: Text -> IO Text
random sep = do
  adverbsFiles <- shuffle [largeAdverbs, mediumAdverbs, smallAdverbs]
  adjectivesFiles <- shuffle [largeAdjectives, mediumAdjectives, smallAdjectives]
  namesFiles <- shuffle [largeNames, mediumNames, smallNames]
  firstLetter <- pick 'a' "abcdefghijklmnopqrstuvwxyz"
  let filterFirstLetter = filter $ T.isPrefixOf $ T.singleton firstLetter
  adverb <- pick "" (filterFirstLetter $ mconcat (T.lines . E.decodeUtf8 <$> adverbsFiles))
  adjective <- pick "" (filterFirstLetter $ mconcat (T.lines . E.decodeUtf8 <$> adjectivesFiles))
  name <- pick "" (filterFirstLetter $ mconcat (T.lines . E.decodeUtf8 <$> namesFiles))
  pure $ T.intercalate sep $ filter (not . T.null) [adverb, adjective, name]

largeAdverbs :: Data.ByteString.ByteString
largeAdverbs = $(embedFile "./words/large/adverbs.txt")

mediumAdverbs :: Data.ByteString.ByteString
mediumAdverbs = $(embedFile "./words/medium/adverbs.txt")

smallAdverbs :: Data.ByteString.ByteString
smallAdverbs = $(embedFile "./words/small/adverbs.txt")

largeAdjectives :: Data.ByteString.ByteString
largeAdjectives = $(embedFile "./words/large/adjectives.txt")

mediumAdjectives :: Data.ByteString.ByteString
mediumAdjectives = $(embedFile "./words/medium/adjectives.txt")

smallAdjectives :: Data.ByteString.ByteString
smallAdjectives = $(embedFile "./words/small/adjectives.txt")

largeNames :: Data.ByteString.ByteString
largeNames = $(embedFile "./words/large/names.txt")

mediumNames :: Data.ByteString.ByteString
mediumNames = $(embedFile "./words/medium/names.txt")

smallNames :: Data.ByteString.ByteString
smallNames = $(embedFile "./words/small/names.txt")

pick :: a -> [a] -> IO a
pick x xs = do
  i <- randomRIO (0, length xs - 1)
  pure $ M.fromMaybe x $ atMay xs i

shuffle :: Eq a => [a] -> IO [a]
shuffle [] = pure []
shuffle xs@(x : _) = do
  y <- pick x xs
  ys <- shuffle (filter (/= y) xs)
  pure (y : ys)
