{-# LANGUAGE TemplateHaskell #-}

module KF5Names where

import Control.Lens
import Data.Map (Map)

import Types

newtype KF5Names = KF5Names { _kf5Names :: Map ByteString ByteString }
makeLenses ''KF5Names
