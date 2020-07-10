{-# LANGUAGE DeriveGeneric #-}

module Data.Environment
    ( Environment()
    , getEnvironment
    , mkEnvironment
    , emptyEnvironment
    , maybeEnvironment
    , lookupEnv)
where

import Hasura.Prelude
import Data.Aeson

import qualified System.Environment
import qualified Data.Map as M

newtype Environment = Environment (M.Map String String) deriving (Eq, Show, Generic)

instance FromJSON Environment

getEnvironment :: IO Environment
getEnvironment = mkEnvironment <$> System.Environment.getEnvironment

maybeEnvironment :: Maybe Environment -> Environment
maybeEnvironment = fromMaybe emptyEnvironment

mkEnvironment :: [(String, String)] -> Environment
mkEnvironment = Environment . M.fromList

emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty

lookupEnv :: Environment -> String -> Maybe String
lookupEnv (Environment es) k = M.lookup k es
