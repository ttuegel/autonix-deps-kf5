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
import KF5

main :: IO ()
main = do
    (deps, kf5) <- runStateT analyze $ KF5 M.empty M.empty
    writeDeps $ map
        ( (_2 %~ renameDeps kf5)
          . propagateDeps kf5
        ) deps
  where
    analyze :: StateT KF5 IO [(ByteString, Deps)]
    analyze = analyzePackages $ \name path -> do
        let analyzers :: [Analyzer (StateT Deps (StateT KF5 IO))]
            analyzers =
                concat
                [ [ resolveKF5Names name
                  , resolveKF5Propagates
                  ]
                , cmakeAnalyzers
                ]
        analyzeFiles analyzers name path
            <&> nativePkgs [ "BISON"
                           , "ECM"
                           , "FLEX"
                           , "KF5DocTools"
                           , "KF5I18N"
                           , "LibXslt"
                           , "Perl"
                           , "PythonInterp"
                           ]
            . userEnvPkgs [ "SharedMimeInfo"
                          ]
            . renameECM

renameECM :: Deps -> Deps
renameECM = execState $ do
    buildInputs %= S.map rename
    nativeBuildInputs %= S.map rename
    propagatedBuildInputs %= S.map rename
    propagatedNativeBuildInputs %= S.map rename
    propagatedUserEnvPkgs %= S.map rename
  where
    rename "ECM" = "extra-cmake-modules"
    rename x = x

nativePkgs :: [ByteString] -> Deps -> Deps
nativePkgs names = execState $ do
    inputs <- filterM (\x -> S.member x <$> use buildInputs) names
    propInputs <- filterM (\x -> S.member x <$> use propagatedBuildInputs) names
    forM_ inputs $ \x -> do
        buildInputs %= S.delete x
        nativeBuildInputs %= S.insert x
    forM_ propInputs $ \x -> do
        propagatedBuildInputs %= S.delete x
        propagatedNativeBuildInputs %= S.insert x

userEnvPkgs :: [ByteString] -> Deps -> Deps
userEnvPkgs names = execState $ do
    inputs <- filterM (\x -> S.member x <$> use buildInputs) names
    propInputs <- filterM (\x -> S.member x <$> use propagatedBuildInputs) names
    forM_ (inputs ++ propInputs) $ \x -> do
        buildInputs %= S.delete x
        propagatedBuildInputs %= S.delete x
        propagatedNativeBuildInputs %= S.insert x
        propagatedUserEnvPkgs %= S.insert x

propagateDeps :: KF5 -> (ByteString, Deps) -> (ByteString, Deps)
propagateDeps kf5 (name, deps) =
    ( name
    , flip execState deps $ do
        case M.lookup name propagate of
            Nothing -> return ()
            Just ds -> propagatedBuildInputs %= S.union ds
    )
  where
    names = kf5^.kf5Names
    propagate = M.mapKeys
                (\x -> M.findWithDefault x x names)
                $ kf5^.kf5Propagate
