module Hasura.RQL.Misc.GetVersion
  where

import           Data.Aeson
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax         (Lift)

import           Hasura.Prelude
import           Hasura.EncJSON
import qualified Hasura.Server.Version as V

data GetVersion
  = GetVersion
  deriving (Show, Eq, Lift)
$(deriveToJSON defaultOptions ''GetVersion)

instance FromJSON GetVersion where
  parseJSON _ = return GetVersion

runGetVersion :: Monad m => GetVersion -> m EncJSON
runGetVersion _ = return $ encJFromJValue $ object [ "version" .= V.currentVersion ]
