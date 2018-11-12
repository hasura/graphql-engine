{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.Types.RemoteSchema where

import           Hasura.Prelude
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Data.HashMap.Strict        as Map
import qualified Network.URI.Extended       as N

import           Hasura.RQL.DDL.Headers     (HeaderConf (..))


type UrlFromEnv = Text

data RemoteSchemaDef
  = RemoteSchemaDef
  { _rsName             :: !Text
  , _rsUrl              :: !(Either N.URI UrlFromEnv)
  , _rsHeaders          :: ![HeaderConf]
  , _rsFwdClientHeaders :: !Bool
  } deriving (Show, Eq, Lift)

type RemoteSchemaMap = Map.HashMap N.URI RemoteSchemaDef

instance J.FromJSON RemoteSchemaDef where
  parseJSON = J.withObject "RemoteSchemaDef" $ \o -> do
    mUrl        <- o J..:? "url"
    mUrlFromEnv <- o J..:? "url_from_env"
    headers     <- o J..: "headers"
    name        <- o J..: "name"
    fwdHdrs     <- o J..: "forward_client_headers"

    eUrlVal <- case (mUrl, mUrlFromEnv) of
      (Just url, Nothing)    -> return $ Left url
      (Nothing, Just urlEnv) -> return $ Right urlEnv
      (Nothing, Nothing)     ->
        fail "both `url` and `url_from_env` can't be empty"
      (Just _, Just _)       ->
        fail "both `url` and `url_from_env` can't be present"

    return $ RemoteSchemaDef name eUrlVal headers fwdHdrs

instance J.ToJSON RemoteSchemaDef where
  toJSON (RemoteSchemaDef name eUrlVal headers fwdHdrs) =
    case eUrlVal of
      Left url ->
        J.object [ "url" J..= url
                 , "headers" J..= headers
                 , "name" J..= name
                 , "forward_client_headers" J..= fwdHdrs
                 ]
      Right urlFromEnv ->
        J.object [ "url_from_env" J..= urlFromEnv
                 , "headers" J..= headers
                 , "name" J..= name
                 , "forward_client_headers" J..= fwdHdrs
                 ]


data AddRemoteSchemaQuery
  = AddRemoteSchemaQuery
  { _arsqUrl                  :: !(Maybe N.URI)
  , _arsqUrlFromEnv           :: !(Maybe Text)
  , _arsqHeaders              :: !(Maybe [HeaderConf])
  , _arsqName                 :: !Text
  , _arsqForwardClientHeaders :: !Bool
  } deriving (Show, Eq, Lift)

data RemoveRemoteSchemaQuery
  = RemoveRemoteSchemaQuery
  { _rrsqName    :: !Text
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaQuery)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''RemoveRemoteSchemaQuery)
