{-# LANGUAGE QuasiQuotes #-}

-- | Schema parsers for logical models.
module Hasura.LogicalModel.Schema (defaultBuildLogicalModelRootFields) where

import Data.Has (Has (getter))
import Data.HashMap.Strict qualified as HM
import Data.Monoid (Ap (Ap, getAp))
import Hasura.GraphQL.Schema.Backend
  ( BackendCustomTypeSelectSchema (..),
    BackendSchema (columnParser),
    MonadBuildSchema,
  )
import Hasura.GraphQL.Schema.Common
  ( SchemaT,
    retrieve,
  )
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
  ( customTypeSelectionList,
  )
import Hasura.LogicalModel.IR (NativeQuery (..))
import Hasura.LogicalModel.Metadata
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (gBoolExpTrue)
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Select (QueryDB (QDBMultipleRows))
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value (UnpreparedValue (UVParameter), openValueOrigin)
import Hasura.RQL.Types.Backend
  ( Backend (ScalarType),
  )
import Hasura.RQL.Types.Column qualified as Column
import Hasura.RQL.Types.Metadata.Object qualified as MO
import Hasura.RQL.Types.Source
  ( SourceInfo (_siCustomization, _siName),
  )
import Hasura.RQL.Types.SourceCustomization
  ( ResolvedSourceCustomization (_rscNamingConvention),
  )
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

defaultBuildLogicalModelRootFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendCustomTypeSelectSchema b
  ) =>
  NativeQueryInfo b ->
  SchemaT
    r
    m
    (Maybe (P.FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
defaultBuildLogicalModelRootFields NativeQueryInfo {..} = runMaybeT $ do
  let fieldName = getLogicalModelName nqiRootFieldName
  logicalModelArgsParser <- logicalModelArgumentsSchema @b @r @m @n fieldName nqiArguments

  sourceInfo :: SourceInfo b <- asks getter

  let sourceName = _siName sourceInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
      description = G.Description <$> nqiDescription

  stringifyNumbers <- retrieve Options.soStringifyNumbers

  selectionSetParser <- MaybeT $ customTypeSelectionList @b @r @m @n (getLogicalModelName nqiRootFieldName) nqiReturns
  customTypesArgsParser <- lift $ customTypeArguments @b @r @m @n (getLogicalModelName nqiRootFieldName) nqiReturns

  let interpolatedQuery nqArgs =
        InterpolatedQuery $
          (fmap . fmap)
            ( \var -> case HM.lookup var nqArgs of
                Just arg -> UVParameter Nothing arg
                Nothing ->
                  -- the `logicalModelArgsParser` will already have checked
                  -- we have all the args the query needs so this _should
                  -- not_ happen
                  error $ "No native query arg passed for " <> show var
            )
            (getInterpolatedQuery nqiCode)

  pure $
    P.setFieldParserOrigin (MO.MOSourceObjId sourceName (mkAnyBackend $ MO.SMOLogicalModel @b nqiRootFieldName)) $
      P.subselection
        fieldName
        description
        ( (,)
            <$> customTypesArgsParser
            <*> logicalModelArgsParser
        )
        selectionSetParser
        <&> \((args, nqArgs), fields) ->
          QDBMultipleRows $
            IR.AnnSelectG
              { IR._asnFields = fields,
                IR._asnFrom =
                  IR.FromNativeQuery
                    NativeQuery
                      { nqRootFieldName = nqiRootFieldName,
                        nqArgs,
                        nqInterpolatedQuery = interpolatedQuery nqArgs
                      },
                IR._asnPerm = IR.TablePerm gBoolExpTrue Nothing,
                IR._asnArgs = args,
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = Just tCase
              }

logicalModelArgumentsSchema ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  G.Name ->
  HashMap NativeQueryArgumentName (ScalarType b) ->
  MaybeT (SchemaT r m) (P.InputFieldsParser n (HashMap NativeQueryArgumentName (Column.ColumnValue b)))
logicalModelArgumentsSchema logicalModelName argsSignature = do
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
                (Just $ G.Description ("Logical model argument " <> getNativeQueryArgumentName name))
                argValueParser
        )
        (HM.toList argsSignature)

  let desc = Just $ G.Description $ G.unName logicalModelName <> " Logical Model Arguments"

  pure $
    if null argsSignature
      then mempty
      else
        P.field
          [G.name|args|]
          desc
          (P.object (logicalModelName <> [G.name|_arguments|]) desc argsParser)
