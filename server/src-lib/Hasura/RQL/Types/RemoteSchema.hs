module Hasura.RQL.Types.RemoteSchema where

import           Hasura.Prelude
import           Language.Haskell.TH.Syntax (Lift)
import           System.Environment         (lookupEnv)

import           Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified Network.URI.Extended       as N

import           Hasura.RQL.DDL.Headers     (HeaderConf (..))
import           Hasura.RQL.Types.Error

type UrlFromEnv = Text
type RemoteSchemaName = Text

data RemoteSchemaInfo
  = RemoteSchemaInfo
  { rsName             :: !RemoteSchemaName
  , rsUrl              :: !N.URI
  , rsHeaders          :: ![HeaderConf]
  , rsFwdClientHeaders :: !Bool
  } deriving (Show, Eq, Lift, Generic)

instance Hashable RemoteSchemaInfo

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''RemoteSchemaInfo)

data RemoteSchemaDef
  = RemoteSchemaDef
  { _rsdUrl                  :: !(Maybe N.URI)
  , _rsdUrlFromEnv           :: !(Maybe UrlFromEnv)
  , _rsdHeaders              :: !(Maybe [HeaderConf])
  , _rsdForwardClientHeaders :: !Bool
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''RemoteSchemaDef)

type RemoteSchemaMap = Map.HashMap RemoteSchemaName RemoteSchemaInfo

data AddRemoteSchemaQuery
  = AddRemoteSchemaQuery
  { _arsqName       :: !RemoteSchemaName -- TODO: name validation: cannot be empty?
  , _arsqDefinition :: !RemoteSchemaDef
  , _arsqComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaQuery)

data RemoveRemoteSchemaQuery
  = RemoveRemoteSchemaQuery
  { _rrsqName    :: !Text
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''RemoveRemoteSchemaQuery)

data GetRemoteSchemaInfoQuery
  = GetRemoteSchemaInfoQuery
  deriving (Show, Eq, Lift)

instance J.FromJSON GetRemoteSchemaInfoQuery where
  parseJSON _ = return GetRemoteSchemaInfoQuery

$(J.deriveToJSON J.defaultOptions ''GetRemoteSchemaInfoQuery)

getUrlFromEnv :: (MonadIO m, MonadError QErr m) => Text -> m N.URI
getUrlFromEnv urlFromEnv = do
  mEnv <- liftIO . lookupEnv $ T.unpack urlFromEnv
  env  <- maybe (throw400 InvalidParams $ envNotFoundMsg urlFromEnv) return
          mEnv
  maybe (throw400 InvalidParams $ invalidUri env) return $ N.parseURI env
  where
    invalidUri uri = "not a valid URI: " <> T.pack uri
    envNotFoundMsg e =
      "environment variable '" <> e <> "' not set"

