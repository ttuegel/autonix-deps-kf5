{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KF5 where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

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
