{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.Monoid

import Autonix.Analyze
import Autonix.Deps
import Autonix.Generate
import Autonix.KF5

main :: IO ()
main = do
    analysis <- flip execStateT mempty $ do
        rename "ECM" "extra-cmake-modules"
        analyzePackages $ analyzeFiles kf5Analyzers
        kf5PostAnalyze
    writeDeps analysis
    writeRenames analysis
