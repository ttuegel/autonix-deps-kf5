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
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath (takeBaseName, takeExtensions, takeFileName)

import Autonix.Analyze
import Autonix.CMake
import Autonix.Package (Package)
import qualified Autonix.Package as Package
import Autonix.Regex

printFilePaths :: MonadIO m => Analyzer m
printFilePaths _ _ = awaitForever $ \(path, _) -> liftIO $ putStrLn path

findKF5Components :: (MonadIO m, MonadState (Map Text Package) m) => Analyzer m
findKF5Components pkg _ = awaitForever $ \(path, contents) ->
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
        ix pkg . Package.buildInputs <>= new

propagateKF5Deps :: (MonadIO m, MonadState (Map Text Package) m) => Analyzer m
propagateKF5Deps pkg _ = awaitForever $ \(path, contents) ->
  when (".cmake" `isPrefixOf` takeExtensions path) $ do
    let base = T.pack $ takeBaseName $ takeBaseName path
        regex = makeRegex
                "find_dependency[[:space:]]*\\([[:space:]]*\
                \([^[:space:],$\\)]+)"
    case T.splitAt (T.length base - 6) base of
      (_, "Config") -> do
        let new = S.fromList
                  $ map (T.toLower . T.decodeUtf8)
                  $ filter (not . cmakeReserved)
                  $ filter (not . B.null)
                  $ concatMap (take 1 . drop 1)
                  $ match regex contents
        ix pkg . Package.propagatedBuildInputs <>= new
      _ -> return ()

findQt5Components :: (MonadIO m, MonadState (Map Text Package) m) => Analyzer m
findQt5Components pkg _ = awaitForever $ \(path, contents) ->
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
        ix pkg . Package.buildInputs <>= new

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
                   , (==) "CTest"
                   ]

kf5Analyzers :: (MonadIO m, MonadState (Map Text Package) m) => [Analyzer m]
kf5Analyzers =
  [ findKF5Components
  , findQt5Components
  , propagateKF5Deps
  ]
  ++ cmakeAnalyzers
