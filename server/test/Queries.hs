{-# LANGUAGE OverloadedStrings #-}

module Queries where

import           Hasura.Prelude

import           Hasura.RQL.DDL.Schema.Table
import           Hasura.Server.Query
import           Hasura.SQL.Types


createTable :: RQLQuery
createTable = RQRunSql $ RunSQL "CREATE TABLE author (id SERIAL PRIMARY KEY, name text)" Nothing

trackTable :: RQLQuery
trackTable = RQTrackTable $ TrackTable $ QualifiedTable (SchemaName "public") (TableName "author")
