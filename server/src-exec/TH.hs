{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module TH
  ( metadataQuery
  , migrateMetadataFrom1
  ) where

import           Language.Haskell.TH.Syntax (Q, TExp, unTypeQ)

import qualified Data.Yaml.TH               as Y

import           Hasura.Server.Query

metadataQuery :: RQLQuery
metadataQuery = $(unTypeQ (Y.decodeFile "src-rsr/hdb_metadata.yaml" :: Q (TExp RQLQuery)))

migrateMetadataFrom1 :: RQLQuery
migrateMetadataFrom1 = $(unTypeQ (Y.decodeFile "src-rsr/migrate_metadata_from_1.yaml" :: Q (TExp RQLQuery)))

