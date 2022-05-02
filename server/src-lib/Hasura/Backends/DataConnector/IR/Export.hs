{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Export
  ( QueryError (..),
    queryToAPI,
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended
import Data.Aeson (ToJSON)
import Data.HashMap.Strict qualified as M
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.Query qualified as IR.Q
import Hasura.Prelude
import Witch qualified

--------------------------------------------------------------------------------

data QueryError = InvalidExpression IR.E.Expression | ExposedLiteral Text
  deriving stock (Generic)
  deriving anyclass (ToJSON)

queryToAPI :: IR.Q.Query -> Either QueryError API.Query
queryToAPI IR.Q.Query {..} = do
  where_' <- traverse expressionToAPI where_
  fields' <- traverse fromField fields
  pure $
    API.Query
      { fields = fields',
        from = Witch.from from,
        limit = limit,
        offset = offset,
        where_ = where_',
        orderBy = nonEmpty $ fmap Witch.from orderBy
      }

fromField :: IR.Q.Field -> Either QueryError API.Field
fromField = \case
  IR.Q.Column contents -> Right $ Witch.from contents
  IR.Q.Relationship contents -> rcToAPI contents
  IR.Q.Literal lit -> Left $ ExposedLiteral lit

rcToAPI :: IR.Q.RelationshipContents -> Either QueryError API.Field
rcToAPI (IR.Q.RelationshipContents joinCondition query) =
  let joinCondition' = M.mapKeys Witch.from $ fmap Witch.from joinCondition
   in fmap (API.RelationshipField . API.RelField joinCondition') $ queryToAPI query

expressionToAPI :: IR.E.Expression -> Either QueryError API.Expression
expressionToAPI = \case
  IR.E.Literal value -> Right $ API.Literal (ValueWrapper (Witch.from value))
  IR.E.In x xs -> do
    x' <- expressionToAPI x
    pure $ API.In (ValueWrapper2 x' (map Witch.from xs))
  IR.E.And exprs -> fmap (API.And . ValueWrapper) $ traverse expressionToAPI exprs
  IR.E.Or exprs -> fmap (API.Or . ValueWrapper) $ traverse expressionToAPI exprs
  IR.E.Not expr -> fmap (API.Not . ValueWrapper) $ expressionToAPI expr
  IR.E.IsNull expr -> fmap (API.IsNull . ValueWrapper) $ expressionToAPI expr
  IR.E.Column name -> Right $ API.Column $ ValueWrapper $ Witch.from name
  IR.E.ApplyOperator op expr1 expr2 -> do
    expr1' <- expressionToAPI expr1
    expr2' <- expressionToAPI expr2
    pure $ API.ApplyOperator $ ValueWrapper3 (Witch.from op) expr1' expr2'
  expr@IR.E.Array {} -> Left $ InvalidExpression expr
