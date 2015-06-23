{-# LANGUAGE OverloadedStrings #-}

module Main where

import Autonix
import Autonix.KF5

main :: IO ()
main = autonix kf5Analyzers
