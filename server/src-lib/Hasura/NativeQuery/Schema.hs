{-# LANGUAGE QuasiQuotes #-}

module Hasura.NativeQuery.Schema (defaultBuildNativeQueryRootFields) where

import Data.Has (Has (getter))
import Data.HashMap.Strict qualified as HM
import Data.Monoid (Ap (Ap, getAp))
import Hasura.GraphQL.Schema.Backend
  ( BackendSchema (columnParser),
    BackendTableSelectSchema (tableArguments),
    MonadBuildSchema,
  )
import Hasura.GraphQL.Schema.Common
  ( SchemaContext (scRole),
    SchemaT,
    askTableInfo,
    retrieve,
  )
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
  ( tablePermissionsInfo,
    tableSelectionList,
  )
import Hasura.GraphQL.Schema.Table (tableSelectPermissions)
import Hasura.NativeQuery.IR (NativeQueryImpl (..))
import Hasura.NativeQuery.Metadata
import Hasura.Prelude
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Select (QueryDB (QDBMultipleRows))
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value (UnpreparedValue (UVParameter), openValueOrigin)
import Hasura.RQL.Types.Backend
  ( Backend (NativeQuery, ScalarType),
  )
import Hasura.RQL.Types.Column qualified as Column
import Hasura.RQL.Types.Metadata.Object qualified as MO
import Hasura.RQL.Types.Source
  ( SourceInfo (_siCustomization, _siName),
  )
import Hasura.RQL.Types.SourceCustomization
  ( ResolvedSourceCustomization (_rscNamingConvention),
  )
import Hasura.RQL.Types.Table (tableInfoName)
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

defaultBuildNativeQueryRootFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b,
    NativeQuery b ~ NativeQueryImpl b
  ) =>
  NativeQueryInfoImpl b ->
  SchemaT
    r
    m
    (Maybe (P.FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
defaultBuildNativeQueryRootFields NativeQueryInfoImpl {..} = runMaybeT $ do
  tableInfo <- askTableInfo @b nqiiReturns
  fieldName <- hoistMaybe (G.mkName $ getNativeQueryNameImpl nqiiRootFieldName)
  nativeQueryArgsParser <- nativeQueryArgumentsSchema @b @r @m @n fieldName nqiiArguments
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
      description = G.Description <$> nqiiDescription
  stringifyNumbers <- retrieve Options.soStringifyNumbers
  roleName <- retrieve scRole

  selectionSetParser <- MaybeT $ tableSelectionList @b @r @m @n tableInfo
  tableArgsParser <- lift $ tableArguments @b @r @m @n tableInfo
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo

  let interpolatedQuery nqArgs =
        InterpolatedQuery $
          (fmap . fmap)
            ( \var -> case HM.lookup var nqArgs of
                Just arg -> UVParameter Nothing arg
                Nothing ->
                  -- the `nativeQueryArgsParser` will already have checked
                  -- we have all the args the query needs so this _should
                  -- not_ happen
                  error $ "No native query arg passed for " <> show var
            )
            (getInterpolatedQuery nqiiCode)

  pure $
    P.setFieldParserOrigin (MO.MOSourceObjId sourceName (mkAnyBackend $ MO.SMOTable @b tableName)) $
      P.subselection fieldName description ((,) <$> tableArgsParser <*> nativeQueryArgsParser) selectionSetParser
        <&> \((args, nqArgs), fields) ->
          QDBMultipleRows $
            IR.AnnSelectG
              { IR._asnFields = fields,
                IR._asnFrom =
                  IR.FromNativeQuery
                    NativeQueryImpl
                      { nqRootFieldName = nqiiRootFieldName,
                        nqArgs,
                        nqInterpolatedQuery = interpolatedQuery nqArgs
                      },
                IR._asnPerm = tablePermissionsInfo selectPermissions,
                IR._asnArgs = args,
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = Just tCase
              }

nativeQueryArgumentsSchema ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  G.Name ->
  HashMap NativeQueryArgumentName (ScalarType b) ->
  MaybeT (SchemaT r m) (P.InputFieldsParser n (HashMap NativeQueryArgumentName (Column.ColumnValue b)))
nativeQueryArgumentsSchema nativeQueryName argsSignature = do
  -- Lift 'SchemaT r m (InputFieldsParser ..)' into a monoid using Applicative.
  -- This lets us use 'foldMap' + monoid structure of hashmaps to avoid awkwardly
  -- traversing the arguments and building the resulting parser.
  argsParser <-
    getAp $
      foldMap
        ( \(name, ty) -> Ap do
            argValueParser <-
              fmap (HM.singleton name . openValueOrigin)
                <$> lift (columnParser (Column.ColumnScalar ty) (G.Nullability False))
            -- TODO: Break in some interesting way if we cannot make a name?
            -- TODO: Naming conventions?
            -- TODO: Custom fields? (Probably not)
            argName <- hoistMaybe (G.mkName (getNativeQueryArgumentName name))
            return $
              P.field
                argName
                (Just $ G.Description ("Native query argument " <> getNativeQueryArgumentName name))
                argValueParser
        )
        (HM.toList argsSignature)

  let desc = Just $ G.Description $ G.unName nativeQueryName <> " Native Query Arguments"

  pure $
    if null argsSignature
      then mempty
      else
        P.field
          [G.name|args|]
          desc
          (P.object (nativeQueryName <> [G.name|_arguments|]) desc argsParser)
