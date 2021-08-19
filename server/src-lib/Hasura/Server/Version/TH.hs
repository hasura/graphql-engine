module Hasura.Server.Version.TH (getVersionFromEnvironment) where

import           Hasura.Prelude

import           Hasura.Server.Utils        (getValFromEnvOrScript)
import           Hasura.Server.Version

import           Data.FileEmbed             (makeRelativeToProject)
import qualified Data.Text                  as T
import           Data.Text.Conversions      (FromText (..))
import qualified Language.Haskell.TH.Syntax as TH

getVersionFromEnvironment :: TH.Q (TH.TExp Version)
getVersionFromEnvironment = do
  txt <- getValFromEnvOrScript "VERSION" <$> makeRelativeToProject "../scripts/get-version.sh"
  [|| fromText $ T.dropWhileEnd (== '\n') $ T.pack $$(txt) ||]
