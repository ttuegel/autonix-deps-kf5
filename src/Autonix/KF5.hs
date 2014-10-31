{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.KF5 where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Set as S
import System.FilePath (takeBaseName, takeExtensions)

import Autonix.Analyze
import Autonix.CMake
import Autonix.Deps
import Autonix.Regex

renameKF5Pkgs :: (MonadIO m, MonadState Deps m) => Analyzer m
renameKF5Pkgs pkg = matchFileName "metainfo.yaml" $ \contents -> do
    let regex = makeRegex "cmakename:[[:space:]]*([[:alnum:]]*)"
    matches <- liftM (match regex) $ liftIO contents
    case matches of
        ((_ : cmakeName : _) : _) -> rename cmakeName pkg
        _ -> return ()

propagateKF5Deps :: (MonadIO m, MonadState Deps m) => Analyzer m
propagateKF5Deps _ path contents = do
    let isCMake = ".cmake" `isPrefixOf` takeExtensions path
        base = takeBaseName $ takeBaseName path
        regex = makeRegex
                "find_dependency[[:space:]]*\\([[:space:]]*([^[:space:],$\\)]+)"
    case splitAt (length base - 6) base of
        (pkg, "Config") | isCMake -> do
            new <- liftM (concatMap (take 1 . drop 1) . match regex)
                   $ liftIO contents
            at (B.pack pkg) %=
                Just
                . (propagatedBuildInputs %~ S.union (S.fromList new))
                . fromMaybe mempty
        _ -> return ()

kf5Analyzers :: (MonadIO m, MonadState Deps m) => [Analyzer m]
kf5Analyzers = [renameKF5Pkgs, propagateKF5Deps] ++ cmakeAnalyzers

kf5PostAnalyze :: MonadState Deps m => m ()
kf5PostAnalyze = do
    moveNativeInputs
    movePropagatedInputs
    moveUserEnvPkgs

moveNativeInputs :: MonadState Deps m => m ()
moveNativeInputs = deps %= M.map makeNative
  where
    makeNative = execState $ forM_ native $ \dep -> do
        hasDep <- use (buildInputs.to (S.member dep))
        when hasDep $ do
            buildInputs %= S.delete dep
            nativeBuildInputs %= S.insert dep
        hasPropDep <- use (propagatedBuildInputs.to (S.member dep))
        when hasPropDep $ do
            propagatedBuildInputs %= S.delete dep
            propagatedNativeBuildInputs %= S.insert dep
    native = [ "BISON"
             , "extra-cmake-modules"
             , "FLEX"
             , "kdoctools"
             , "ki18n"
             , "LibXslt"
             , "Perl"
             , "PythonInterp"
             ]

movePropagatedInputs :: MonadState Deps m => m ()
movePropagatedInputs = deps %= M.map propagate
  where
    propagate = execState $ forM_ propagated $ \dep -> do
        hasDep <- use (buildInputs.to (S.member dep))
        when hasDep $ do
            buildInputs %= S.delete dep
            propagatedBuildInputs %= S.insert dep
        hasNativeDep <- use (nativeBuildInputs.to (S.member dep))
        when hasNativeDep $ do
            nativeBuildInputs %= S.delete dep
            propagatedNativeBuildInputs %= S.insert dep
    propagated = [ "extra-cmake-modules" ]

moveUserEnvPkgs :: MonadState Deps m => m ()
moveUserEnvPkgs = deps %= M.map propagate
  where
    propagate = execState $ forM_ userEnv $ \dep -> do
        hasDep <- use (buildInputs.to (S.member dep))
        hasNativeDep <- use (buildInputs.to (S.member dep))
        hasPropDep <- use (buildInputs.to (S.member dep))
        hasPropNativeDep <- use (buildInputs.to (S.member dep))
        when (hasDep || hasNativeDep || hasPropDep || hasPropNativeDep)
            $ propagatedUserEnvPkgs %= S.insert dep
    userEnv = [ "SharedMimeInfo" ]
