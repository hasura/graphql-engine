module Hasura.Server.Version.TH (getVersionFromEnvironment) where

import Data.FileEmbed (makeRelativeToProject)
import Data.Text qualified as T
import Data.Text.Conversions (FromText (..))
import Hasura.Prelude
import Hasura.Server.Utils (getValFromEnvOrScript)
import Hasura.Server.Version
import Language.Haskell.TH.Syntax qualified as TH

getVersionFromEnvironment :: TH.Q (TH.TExp Version)
getVersionFromEnvironment = do
  txt <- getValFromEnvOrScript "VERSION" <$> makeRelativeToProject "../scripts/get-version.sh"
  [||fromText $ T.dropWhileEnd (== '\n') $ T.pack $$(txt)||]
