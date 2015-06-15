{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.KF5 where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath (takeFileName)

import Autonix.Analyze
import Autonix.CMake
import Autonix.Package (Package)
import qualified Autonix.Package as Package
import Autonix.Regex
import Autonix.Renames

printFilePaths :: MonadIO m => Analyzer m
printFilePaths _ = awaitForever $ \(path, _) -> liftIO $ putStrLn path

renameKF5Pkgs :: (MonadIO m, MonadState Renames m) => Analyzer m
renameKF5Pkgs pkg = awaitForever $ \(path, contents) ->
  when (takeFileName path == "metainfo.yaml") $ do
    let regex = makeRegex "cmakename:[[:space:]]*([[:alnum:]]*)"
    case match regex contents of
      ((_ : cmakeName : _) : _) ->
        lift $ lift $ lift $ rename (T.toLower $ T.decodeUtf8 cmakeName) pkg
      _ -> return ()

findKF5Components :: MonadIO m => Analyzer m
findKF5Components _ = awaitForever $ \(path, contents) ->
    when ("CMakeLists.txt" == takeFileName path) $ do
        let new = S.fromList
                  $ map ("kf5" <>)
                  $ map (T.toLower . T.decodeUtf8)
                  $ filter (not . cmakeReserved)
                  $ filter (not . B.null)
                  $ concatMap B.words
                  $ concatMap (take 1 . drop 1)
                  $ match regex contents
            regex = makeRegex
                    "find_package[[:space:]]*\\([[:space:]]*KF5\
                    \[[:space:]]*([#\\.${}_[:alnum:][:space:]]+)\\)"
        Package.buildInputs %= S.union new

findQt5Components :: MonadIO m => Analyzer m
findQt5Components _ = awaitForever $ \(path, contents) ->
    when ("CMakeLists.txt" == takeFileName path) $ do
        let new = S.fromList
                  $ map ("qt5" <>)
                  $ map (T.toLower . T.decodeUtf8)
                  $ filter (not . cmakeReserved)
                  $ filter (not . B.null)
                  $ concatMap B.words
                  $ concatMap (take 1 . drop 1)
                  $ match regex contents
            regex = makeRegex
                    "find_package[[:space:]]*\\([[:space:]]*Qt5\
                    \[[:space:]]*([#\\.${}_[:alnum:][:space:]]+)\\)"
        Package.buildInputs %= S.union new

cmakeReserved :: ByteString -> Bool
cmakeReserved bs = or $ map ($ bs)
                   [ B.elem '$'
                   , B.elem '{'
                   , B.elem '}'
                   , B.elem '#'
                   , B.elem '.'
                   , (==) "COMPONENTS"
                   , (==) "REQUIRED"
                   , (==) "CONFIG"
                   , (==) "NO_MODULE"
                   , (==) "QUIET"
                   ]

kf5Analyzers :: (MonadIO m, MonadState Renames m) => [Analyzer m]
kf5Analyzers =
  [ findKF5Components
  , findQt5Components
  , renameKF5Pkgs
  ]
  ++ cmakeAnalyzers

postProcess :: Map Text Package -> Map Text Package
postProcess =
  M.map propagateInputs
  . M.map nativateInputs
  . M.map enviateInputs
  where
    enviateInputs pkg = flip execState pkg $ do
      env <- S.intersection userEnvPackages <$> use Package.buildInputs
      Package.propagatedUserEnvPkgs <>= env

    nativateInputs pkg = flip execState pkg $ do
      native <- S.intersection nativePackages <$> use Package.buildInputs
      Package.nativeBuildInputs <>= native
      Package.buildInputs %= S.filter (\dep -> not $ S.member dep native)

    propagateInputs pkg = flip execState pkg $ do
      let propagated = S.intersection propagatedPackages

      build <- propagated <$> use Package.buildInputs
      Package.propagatedBuildInputs <>= build
      Package.buildInputs %= S.filter (\dep -> not $ S.member dep build)

      native <- propagated <$> use Package.nativeBuildInputs
      Package.propagatedNativeBuildInputs <>= native
      Package.nativeBuildInputs %= S.filter (\dep -> not $ S.member dep native)

nativePackages :: Set Text
nativePackages =
  S.fromList
    [ "bison"
    , "extra-cmake-modules"
    , "flex"
    , "kdoctools"
    , "ki18n"
    , "libxslt"
    , "perl"
    , "pythoninterp"
    ]

propagatedPackages :: Set Text
propagatedPackages =
  S.fromList
    [ "extra-cmake-modules"
    ]

userEnvPackages :: Set Text
userEnvPackages =
  S.fromList
    [ "sharedmimeinfo"
    ]
