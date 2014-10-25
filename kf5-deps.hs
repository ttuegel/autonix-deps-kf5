{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

import Analyze
import CMake
import Deps
import Generate
import KF5Names

main :: IO ()
main = do
    (deps, names) <- runStateT analyze $ KF5Names M.empty
    writeDeps $ map (_2 %~ renameDeps names) deps
  where
    analyze :: StateT KF5Names IO [(ByteString, Deps)]
    analyze = analyzePackages $ \name path -> do
        let analyzers :: [Analyzer (StateT Deps (StateT KF5Names IO))]
            analyzers =
                concat
                [ [resolveKF5Names name]
                , cmakeAnalyzers
                ]
        renameECM <$> analyzeFiles analyzers name path

renameECM :: Deps -> Deps
renameECM = execState $ do
    hasECM <- S.member "ECM" <$> use buildInputs
    when hasECM $ do
        buildInputs %= S.delete "ECM"
        nativeBuildInputs %= S.insert "extra-cmake-modules"
