{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Export
  ( QueryError (..),
    queryRequestToAPI,
  )
where

import Autodocodec.Extended (ValueWrapper (..))
import Data.Aeson (ToJSON)
import Data.Aeson.KeyMap (fromHashMapText)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Query qualified as IR.Q
import Hasura.Prelude
import Witch qualified

--------------------------------------------------------------------------------

data QueryError = ExposedLiteral Text
  deriving stock (Generic)
  deriving anyclass (ToJSON)

queryRequestToAPI :: IR.Q.QueryRequest -> Either QueryError API.QueryRequest
queryRequestToAPI IR.Q.QueryRequest {..} = do
  query <- queryToAPI _qrQuery
  pure $
    API.QueryRequest
      { _qrTable = Witch.from _qrTable,
        _qrTableRelationships =
          ( \(sourceTableName, relationships) ->
              API.TableRelationships
                { _trSourceTable = Witch.from sourceTableName,
                  _trRelationships = HashMap.mapKeys Witch.from $ Witch.from <$> relationships
                }
          )
            <$> HashMap.toList _qrTableRelationships,
        _qrQuery = query
      }

queryToAPI :: IR.Q.Query -> Either QueryError API.Query
queryToAPI IR.Q.Query {..} = do
  fields' <- traverse fieldToAPI _qFields
  pure $
    API.Query
      { _qFields = fromHashMapText fields',
        _qLimit = _qLimit,
        _qOffset = _qOffset,
        _qWhere = fmap Witch.from _qWhere,
        _qOrderBy = nonEmpty $ fmap Witch.from _qOrderBy
      }

fieldToAPI :: IR.Q.Field -> Either QueryError API.Field
fieldToAPI = \case
  IR.Q.ColumnField contents -> Right . API.ColumnField . ValueWrapper $ Witch.from contents
  IR.Q.RelField contents -> API.RelField <$> rcToAPI contents
  IR.Q.LiteralField lit -> Left $ ExposedLiteral lit

rcToAPI :: IR.Q.RelationshipField -> Either QueryError API.RelationshipField
rcToAPI IR.Q.RelationshipField {..} = do
  query <- queryToAPI _rfQuery
  pure $
    API.RelationshipField
      { _rfRelationship = Witch.from _rfRelationship,
        _rfQuery = query
      }
