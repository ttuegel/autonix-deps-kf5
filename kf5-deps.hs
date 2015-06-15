{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State

import Autonix.Generate
import Autonix.KF5

main :: IO ()
main = flip evalStateT mempty $ do
  extraRenames
  generateDeps kf5Analyzers postProcess
