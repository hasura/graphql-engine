{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Parser.Column
  ( PGColumnValue(..)
  , column
  , mkColumnTypeName
  , mkScalarTypeName
  , qualifiedObjectToName

  , UnpreparedValue(..)

  , Opaque
  , openOpaque
  , mkParameter
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended          as M
import qualified Database.PG.Query                     as Q

import           Language.GraphQL.Draft.Syntax         (Description (..), Name, Nullability (..),
                                                        Value (..), litName, literal, mkName,
                                                        unsafeMkName)

import qualified Hasura.RQL.Types.Column               as RQL

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Internal.Parser
import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.Types.Column               hiding (EnumValue (..), EnumValueInfo (..))
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Permission           (SessVar)
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           Hasura.SQL.Value

-- -------------------------------------------------------------------------------------------------

data Opaque a = Opaque
  { _opVariable :: Maybe VariableInfo
  -- ^ The variable this value came from, if any.
  , _opValue    :: a
  } -- Note: we intentionally don’t derive any instances here, since that would
    -- defeat the opaqueness!

openOpaque :: MonadParse m => Opaque a -> m a
openOpaque (Opaque Nothing  value) = pure value
openOpaque (Opaque (Just _) value) = markNotReusable $> value

data UnpreparedValue
  -- | A SQL value that can be parameterized over.
  = UVParameter PGColumnValue
                (Maybe VariableInfo)
                -- ^ The GraphQL variable this value came from, if any.
  -- | A literal SQL expression that /cannot/ be parameterized over.
  | UVLiteral SQLExp
  -- | The entire session variables JSON object.
  | UVSession
  -- | A single session variable.
  | UVSessionVar (PGType PGScalarType) SessVar

data PGColumnValue = PGColumnValue
  { pcvType  :: PGColumnType
  , pcvValue :: WithScalarType PGScalarValue
  }

mkParameter :: Opaque PGColumnValue -> UnpreparedValue
mkParameter (Opaque variable value) = UVParameter value variable

-- -------------------------------------------------------------------------------------------------

column
  :: (MonadSchema n m, MonadError QErr m)
  => PGColumnType
  -> Nullability
  -> m (Parser 'Both n (Opaque PGColumnValue))
column columnType (Nullability isNullable) =
  -- TODO: It might be worth memoizing this function even though it isn’t
  -- recursive simply for performance reasons, since it’s likely to be hammered
  -- during schema generation. Need to profile to see whether or not it’s a win.
  opaque . fmap (PGColumnValue columnType) <$> case columnType of
    PGColumnScalar scalarType -> withScalarType scalarType <$> case scalarType of
      PGInteger -> pure (PGValInteger <$> int)
      PGBoolean -> pure (PGValBoolean <$> boolean)
      PGFloat   -> pure (PGValDouble  <$> float)
      PGText    -> pure (PGValText    <$> string)
      PGVarchar -> pure (PGValVarchar <$> string)
      PGJSON    -> pure (PGValJSON  . Q.JSON  <$> json)
      PGJSONB   -> pure (PGValJSONB . Q.JSONB <$> json)
      _         -> do
        name <- mkColumnTypeName columnType
        pure (PGValUnknown <$> scalar name Nothing SRString) -- FIXME: is SRString right?
    PGColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (M.toList enumValues) of
        Just enumValuesList -> do
          name <- mkColumnTypeName columnType
          pure $ withScalarType PGText $ enum name Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> error "empty enum values" -- FIXME
  where
    -- Sadly, this combinator is not sound in general, so we can’t export it
    -- for general-purpose use. If we did, someone could write this:
    --
    --   mkParameter <$> opaque do
    --     n <- int
    --     pure (mkIntColumnValue (n + 1))
    --
    -- Now we’d end up with a UVParameter that has a variable in it, so we’d
    -- parameterize over it. But when we’d reuse the plan, we wouldn’t know to
    -- increment the value by 1, so we’d use the wrong value!
    --
    -- We could theoretically solve this by retaining a reference to the parser
    -- itself and re-parsing each new value, using the saved parser, which
    -- would admittedly be neat. But it’s more complicated, and it isn’t clear
    -- that it would actually be useful, so for now we don’t support it.
    opaque :: Functor m => Parser 'Both m a -> Parser 'Both m (Opaque a)
    opaque parser = parser
      { pParser = \case
          VVariable Variable{ vInfo, vValue } ->
            Opaque (Just vInfo) <$> pParser parser (literal vValue)
          value -> Opaque Nothing <$> pParser parser value
      }

    withScalarType scalarType = fmap (WithScalarType scalarType) . possiblyNullable scalarType
    possiblyNullable scalarType
      | isNullable = fmap (fromMaybe $ PGNull scalarType) . nullable
      | otherwise  = id

    -- FIXME: unify these types, avoid unsafe conversion to Name
    mkEnumValue (RQL.EnumValue value, RQL.EnumValueInfo description) =
      ( mkDefinition (unsafeMkName value) (Description <$> description) EnumValueInfo
      , PGValText value )

mkColumnTypeName :: MonadError QErr m => PGColumnType -> m Name
mkColumnTypeName = \case
  PGColumnScalar scalarType -> mkScalarTypeName scalarType
  PGColumnEnumReference (EnumReference tableName _) ->
    qualifiedObjectToName tableName <&> (<> $$(litName "_enum"))

mkScalarTypeName :: MonadError QErr m => PGScalarType -> m Name
mkScalarTypeName PGInteger  = pure $$(litName "Int")
mkScalarTypeName PGBoolean  = pure $$(litName "Boolean")
mkScalarTypeName PGFloat    = pure $$(litName "Float")
mkScalarTypeName PGText     = pure $$(litName "String")
mkScalarTypeName PGVarchar  = pure $$(litName "String")
mkScalarTypeName scalarType = mkName (toSQLTxt scalarType) `onNothing` throw400 ValidationFailed
  ("cannot use SQL type " <> scalarType <<> " in the GraphQL schema because its name is not a "
  <> "valid GraphQL identifier")

-- FIXME: this doesn't belong here
qualifiedObjectToName :: (ToTxt a, MonadError QErr m) => QualifiedObject a -> m Name
qualifiedObjectToName objectName = do
  let textName = snakeCaseQualObject objectName
  mkName textName `onNothing` throw400 ValidationFailed
    ("cannot include " <> objectName <<> " in the GraphQL schema because " <> textName
    <<> " is not a valid GraphQL identifier")
