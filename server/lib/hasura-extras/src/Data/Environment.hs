{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Environment
  ( Environment (),
    getEnvironment,
    mkEnvironment,
    emptyEnvironment,
    maybeEnvironment,
    lookupEnv,
    redactEnv,
    Data.Environment.toList,
  )
where

import Data.Aeson
import Data.Map qualified as M
import Hasura.Prelude
import System.Environment qualified

-- | Server process environment variables
newtype Environment = Environment (M.Map String String) deriving (Eq, Show, Generic, ToJSON, Semigroup, Monoid)

instance FromJSON Environment

-- 'getEnvironment' is allowed to use 'System.Environment.getEnvironment'
{-# ANN getEnvironment ("HLINT: ignore avoid getEnvironment" :: String) #-}
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

toList :: Environment -> [(String, String)]
toList (Environment e) = M.toList e

redactEnv :: Environment -> Environment
redactEnv (Environment e) = Environment $ fmap (const "<ENV REDACTED>") e
