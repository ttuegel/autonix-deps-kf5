{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S

import Autonix.Analyze
import Autonix.CMake
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
