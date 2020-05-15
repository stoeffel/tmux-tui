module Main (main) where

import Protolude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Runners.Reporter as Reporter

main = defaultMainWithIngredients [Reporter.ingredient] tests

tests :: TestTree
tests =
  testGroup
    "TmuxTui"
    [ testCase "TODO" $
        1 @?= 1
    ]
