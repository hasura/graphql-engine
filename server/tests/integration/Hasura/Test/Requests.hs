{-# LANGUAGE QuasiQuotes #-}

-- | A module collecting common HTTP requests to graphql-engine for testing.
module Hasura.Test.Requests
  ( replaceMetadata,
    postgresRunSql,
    v1graphql,
  )
where

import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import Hasura.Prelude
import Hasura.RQL.Types.Metadata (Metadata)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Test (SResponse)
import Test.Hspec.Wai (WaiSession)
import Test.Hspec.Wai qualified as HW

--------------------------------------------------------------------------------

replaceMetadata :: Metadata -> WaiSession st SResponse
replaceMetadata metadata =
  HW.post "/v1/metadata" $
    encode
      [aesonQQ|
  { type: "replace_metadata"
  , args: { metadata: #{metadata} }
  }
|]

postgresRunSql :: Text -> WaiSession st SResponse
postgresRunSql rawSql =
  HW.post "/v2/query" $
    encode
      [aesonQQ|
  { type: "run_sql"
  , args: { sql: #{rawSql} }
  }
|]

v1graphql :: [Header] -> String -> WaiSession st SResponse
v1graphql headers graphqlQuery =
  HW.request methodPost "/v1/graphql" headers $
    encode
      [aesonQQ|
  { query: #{graphqlQuery} }
|]
