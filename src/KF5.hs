{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KF5 where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Set as S
import System.FilePath

import Analyze
import Regex
import Types

data KF5 =
    KF5
    { _kf5Names :: Map ByteString ByteString
    , _kf5Propagate :: Map ByteString (Set ByteString)
    }
makeLenses ''KF5

resolveKF5Names :: (Functor m, MonadIO m, MonadState KF5 m)
                => ByteString -> Analyzer (StateT Deps m)
resolveKF5Names name = matchFileName "metainfo.yaml" $ \contents -> do
    let regex = makeRegex "cmakename:[[:space:]]*([[:alnum:]]*)"
    matches <- match regex <$> liftIO contents
    case matches of
        ((_ : cmakeName : _) : _) -> lift $ kf5Names %= M.insert cmakeName name
        _ -> return ()

renameDeps :: KF5 -> Deps -> Deps
renameDeps names = execState $ do
    buildInputs %= rename
    propagatedBuildInputs %= rename
    nativeBuildInputs %= rename
    propagatedNativeBuildInputs %= rename
    propagatedUserEnvPkgs %= rename
  where
    rename = S.map $ \x -> M.findWithDefault x x $ names^.kf5Names

resolveKF5Propagates :: (Functor m, MonadIO m, MonadState KF5 m)
                     => Analyzer (StateT Deps m)
resolveKF5Propagates path contents
    | takeExtensions path == ".cmake" || takeExtensions path == ".cmake.in" =
        fromMaybe (return ()) $ do
            name <- stripSuffix "Config" (takeBaseName path)
            return $ do
                let regex = makeRegex "find_dependency\\([[:space:]]*([^[:space:],$\\)]+)"
                new <- concatMap (take 1 . drop 1) . match regex <$> liftIO contents
                lift $ forM_ new $ \x -> do
                    liftIO $ putStrLn "fucker"
                    kf5Propagate . ix (B.pack name) %= S.insert x
    | otherwise = return ()

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suf = fmap reverse . stripPrefix suf . reverse
