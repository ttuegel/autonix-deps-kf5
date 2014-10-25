module Main where

import CMake
import Deps

main :: IO ()
main = generateDeps cmakeAnalyzers
