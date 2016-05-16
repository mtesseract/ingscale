{-# LANGUAGE OverloadedStrings #-}

module Main where

--import           Data.List
--import           Data.Monoid (mempty)
import           Data.Ratio
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T (unpack, concat)
import           Ingscale
--import           Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import           Test.Framework (Test, defaultMain, testGroup)
--import           Test.Framework.Options (TestOptions, TestOptions'(..))
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)
--import           Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import           Test.HUnit
--import           Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = []
