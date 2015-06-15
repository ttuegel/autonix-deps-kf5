{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State

import Autonix.Generate
import Autonix.KF5
import Autonix.Renames

main :: IO ()
main = flip evalStateT mempty $ do
  rename "ecm" "extra-cmake-modules"
  generateDeps kf5Analyzers postProcess
