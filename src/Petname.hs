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
  adverbsFile <- pick smallAdverbs [largeAdverbs, mediumAdverbs, smallAdverbs]
  adjectivesFile <- pick smallAdjectives [largeAdjectives, mediumAdjectives, smallAdjectives]
  namesFile <- pick smallNames [largeNames, mediumNames, smallNames]
  adverb <- pick "" (T.lines $ E.decodeUtf8 adverbsFile)
  adjective <- pick "" (T.lines $ E.decodeUtf8 adjectivesFile)
  name <- pick "" (T.lines $ E.decodeUtf8 namesFile)
  pure $ T.intercalate sep [adverb, adjective, name]

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
