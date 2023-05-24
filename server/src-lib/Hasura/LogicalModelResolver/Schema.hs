{-# LANGUAGE QuasiQuotes #-}

-- | Schema parsers for common functionality of logical model resolvers.
module Hasura.LogicalModelResolver.Schema (argumentsSchema) where

import Data.HashMap.Strict qualified as HashMap
import Data.Monoid (Ap (Ap, getAp))
import Hasura.GraphQL.Schema.Backend
  ( BackendSchema (columnParser),
    MonadBuildSchema,
  )
import Hasura.GraphQL.Schema.Common
  ( SchemaT,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.IR.Value (openValueOrigin)
import Hasura.RQL.Types.Column qualified as Column
import Hasura.StoredProcedure.Metadata (ArgumentName (..))
import Hasura.StoredProcedure.Types (NullableScalarType (..))
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

-- | Schema parser for native query or stored procedure arguments.
argumentsSchema ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  -- | The resolver description, such as "Stored Procedure" or "Native Query".
  Text ->
  -- | The resolver name.
  G.Name ->
  -- | Arguments
  HashMap ArgumentName (NullableScalarType b) ->
  MaybeT (SchemaT r m) (P.InputFieldsParser n (HashMap ArgumentName (Column.ColumnValue b)))
argumentsSchema resolverDesc resolverName argsSignature = do
  -- Lift 'SchemaT r m (InputFieldsParser ..)' into a monoid using Applicative.
  -- This lets us use 'foldMap' + monoid structure of hashmaps to avoid awkwardly
  -- traversing the arguments and building the resulting parser.
  argsParser <-
    getAp
      $ foldMap
        ( \(name, NullableScalarType {nstType, nstNullable, nstDescription}) -> Ap do
            argValueParser <-
              fmap (HashMap.singleton name . openValueOrigin)
                <$> lift (columnParser (Column.ColumnScalar nstType) (G.Nullability nstNullable))
            -- TODO: Naming conventions?
            -- TODO: Custom fields? (Probably not)
            argName <- hoistMaybe (G.mkName (getArgumentName name))
            let description = case nstDescription of
                  Just desc -> G.Description desc
                  Nothing -> G.Description (resolverDesc <> " argument " <> getArgumentName name)
            pure
              $ P.field
                argName
                (Just description)
                argValueParser
        )
        (HashMap.toList argsSignature)

  let desc = Just $ G.Description $ G.unName resolverName <> resolverDesc <> " Arguments"

  pure
    $ if null argsSignature
      then mempty
      else
        P.field
          [G.name|args|]
          desc
          (P.object (resolverName <> [G.name|_arguments|]) desc argsParser)
