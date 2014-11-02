{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.Monoid

import Autonix.Analyze
import Autonix.Args
import Autonix.Deps
import Autonix.Generate
import Autonix.KF5

main :: IO ()
main = withArgs $ \manifest renames -> flip evalStateT mempty $ do
    rename "ECM" "extra-cmake-modules"
    analyzePackages (analyzeFiles kf5Analyzers) manifest renames
    kf5PostAnalyze
    get >>= writeDeps
    get >>= writeRenames
