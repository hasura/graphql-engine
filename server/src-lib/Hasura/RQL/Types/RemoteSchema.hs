module Hasura.RQL.Types.RemoteSchema where

import           Hasura.Prelude
import           Language.Haskell.TH.Syntax (Lift)
import           System.Environment         (lookupEnv)

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
  { rsUrl              :: !N.URI
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

-- instance J.ToJSON RemoteSchemaDef where
--   toJSON (RemoteSchemaDef name eUrlVal headers fwdHdrs) =
--     case eUrlVal of
--       Left url ->
--         J.object [ "url" J..= url
--                  , "headers" J..= headers
--                  , "name" J..= name
--                  , "forward_client_headers" J..= fwdHdrs
--                  ]
--       Right urlFromEnv ->
--         J.object [ "url_from_env" J..= urlFromEnv
--                  , "headers" J..= headers
--                  , "name" J..= name
--                  , "forward_client_headers" J..= fwdHdrs
--                  ]


data AddRemoteSchemaQuery
  = AddRemoteSchemaQuery
  { _arsqName       :: !RemoteSchemaName -- TODO: name validation: cannot be empty?
  , _arsqDefinition :: !RemoteSchemaDef
  , _arsqComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaQuery)

-- data AddRemoteSchemaQuery'
--   = AddRemoteSchemaQuery'
--   { _arsqUrl                  :: !(Maybe N.URI)
--   , _arsqUrlFromEnv           :: !(Maybe Text)
--   , _arsqHeaders              :: !(Maybe [HeaderConf])
--   , _arsqForwardClientHeaders :: !Bool
--   } deriving (Show, Eq, Lift)

data RemoveRemoteSchemaQuery
  = RemoveRemoteSchemaQuery
  { _rrsqName    :: !Text
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''RemoveRemoteSchemaQuery)

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

validateRemoteSchemaDef
  :: (MonadError QErr m, MonadIO m)
  => RemoteSchemaDef
  -> m RemoteSchemaInfo
validateRemoteSchemaDef (RemoteSchemaDef mUrl mUrlEnv hdrC fwdHdrs) =
  case (mUrl, mUrlEnv) of
    (Just url, Nothing)    ->
      return $ RemoteSchemaInfo url hdrs fwdHdrs
    (Nothing, Just urlEnv) -> do
      url <- getUrlFromEnv urlEnv
      return $ RemoteSchemaInfo url hdrs fwdHdrs
    (Nothing, Nothing)     ->
        throw400 InvalidParams "both `url` and `url_from_env` can't be empty"
    (Just _, Just _)       ->
        throw400 InvalidParams "both `url` and `url_from_env` can't be present"
  where hdrs = fromMaybe [] hdrC
