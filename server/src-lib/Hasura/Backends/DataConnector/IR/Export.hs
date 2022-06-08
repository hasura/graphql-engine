{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Export
  ( QueryError (..),
    queryToAPI,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON)
import Data.Aeson.KeyMap (fromHashMapText)
import Data.HashMap.Strict qualified as M
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Query qualified as IR.Q
import Hasura.Prelude
import Witch qualified

--------------------------------------------------------------------------------

data QueryError = ExposedLiteral Text
  deriving stock (Generic)
  deriving anyclass (ToJSON)

queryToAPI :: IR.Q.Query -> Either QueryError API.Query
queryToAPI IR.Q.Query {..} = do
  fields' <- traverse fromField fields
  pure $
    API.Query
      { fields = fromHashMapText fields',
        from = Witch.from from,
        limit = limit,
        offset = offset,
        where_ = fmap Witch.from where_,
        orderBy = nonEmpty $ fmap Witch.from orderBy
      }

fromField :: IR.Q.Field -> Either QueryError API.Field
fromField = \case
  IR.Q.Column contents -> Right $ Witch.from contents
  IR.Q.Relationship contents -> rcToAPI contents
  IR.Q.Literal lit -> Left $ ExposedLiteral lit

rcToAPI :: IR.Q.RelationshipContents -> Either QueryError API.Field
rcToAPI (IR.Q.RelationshipContents joinCondition relType query) =
  let joinCondition' = M.mapKeys Witch.from $ fmap Witch.from joinCondition
   in fmap (API.RelationshipField . API.RelField joinCondition' (Witch.from relType)) $ queryToAPI query
