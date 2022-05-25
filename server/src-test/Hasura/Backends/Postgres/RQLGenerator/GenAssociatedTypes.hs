--
module Hasura.Backends.Postgres.RQLGenerator.GenAssociatedTypes
  ( genBooleanOperators,
    genColumn,
    genFunctionName,
    genScalarType,
    genTableName,
    genXComputedField,
    genFunctionArgumentExp,
  )
where

import Control.Applicative (pure, (<*>))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Text (Text)
import Data.Text qualified as T
import Hasura.Backends.Postgres.Instances.Types ()
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Backends.Postgres.SQL.Types qualified as PGTypes
import Hasura.Backends.Postgres.Types.BoolExp qualified as B
import Hasura.Backends.Postgres.Types.Function qualified as PGTypes
import Hasura.Generator.Common (defaultRange, genArbitraryUnicodeText)
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend (BackendType (..), PostgresKind (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------
-- Exported

genColumn :: MonadGen m => m (Column ('Postgres 'Vanilla))
genColumn = PG.unsafePGCol <$> genArbitraryUnicodeText defaultRange

-- | Generator for a qualified Postgres 'TableName'
genTableName :: MonadGen m => m (TableName ('Postgres 'Vanilla))
genTableName = genQualifiedTable

genScalarType :: MonadGen m => m (ScalarType ('Postgres 'Vanilla))
genScalarType =
  Gen.choice
    [ pure PG.PGSmallInt,
      pure PG.PGInteger,
      pure PG.PGBigInt,
      pure PG.PGSerial,
      pure PG.PGBigSerial,
      pure PG.PGFloat,
      pure PG.PGDouble,
      pure PG.PGNumeric,
      pure PG.PGMoney,
      pure PG.PGBoolean,
      pure PG.PGChar,
      pure PG.PGVarchar,
      pure PG.PGText,
      pure PG.PGCitext,
      pure PG.PGDate,
      pure PG.PGTimeStamp,
      pure PG.PGTimeStampTZ,
      pure PG.PGTimeTZ,
      pure PG.PGJSON,
      pure PG.PGJSONB,
      pure PG.PGGeometry,
      pure PG.PGGeography,
      pure PG.PGRaster,
      pure PG.PGUUID,
      pure PG.PGLtree,
      pure PG.PGLquery,
      pure PG.PGLtxtquery,
      PG.PGUnknown <$> genArbitraryUnicodeText defaultRange,
      PG.PGCompositeScalar <$> genArbitraryUnicodeText defaultRange
    ]

genFunctionName :: MonadGen m => m (FunctionName ('Postgres 'Vanilla))
genFunctionName =
  PG.QualifiedObject <$> genSchemaName defaultRange <*> genPgFunctionName defaultRange

genXComputedField :: MonadGen m => m (XComputedField ('Postgres 'Vanilla))
genXComputedField = pure ()

genBooleanOperators :: MonadGen m => m a -> m (BooleanOperators ('Postgres 'Vanilla) a)
genBooleanOperators genA =
  Gen.choice
    [ B.AILIKE <$> genA,
      B.AILIKE <$> genA,
      B.ANILIKE <$> genA,
      B.ASIMILAR <$> genA,
      B.ANSIMILAR <$> genA,
      B.AREGEX <$> genA,
      B.AIREGEX <$> genA,
      B.ANREGEX <$> genA,
      B.ANIREGEX <$> genA,
      B.AContains <$> genA,
      B.AContainedIn <$> genA,
      B.AHasKey <$> genA,
      B.AHasKeysAny <$> genA,
      B.AHasKeysAll <$> genA,
      B.ASTContains <$> genA,
      B.ASTCrosses <$> genA,
      B.ASTEquals <$> genA,
      B.ASTIntersects <$> genA,
      B.AST3DIntersects <$> genA,
      B.ASTOverlaps <$> genA,
      B.ASTTouches <$> genA,
      B.ASTWithin <$> genA,
      B.ASTIntersectsRast <$> genA,
      B.ASTDWithinGeom <$> genDWithinGeomOp genA,
      B.AST3DDWithinGeom <$> genDWithinGeomOp genA,
      B.ASTDWithinGeog <$> genDWithinGeogOp genA,
      B.ASTIntersectsGeomNband <$> genTIntersectsGeomminNband genA,
      B.ASTIntersectsNbandGeom <$> genSTIntersectsNbandGeommin genA,
      B.AAncestor <$> genA,
      B.AAncestorAny <$> genA,
      B.ADescendant <$> genA,
      B.ADescendantAny <$> genA,
      B.AMatches <$> genA,
      B.AMatchesAny <$> genA,
      B.AMatchesFulltext <$> genA
    ]

genFunctionArgumentExp :: MonadGen m => m a -> m (FunctionArgumentExp ('Postgres 'Vanilla) a)
genFunctionArgumentExp genA =
  Gen.choice
    [ pure PGTypes.AETableRow,
      pure PGTypes.AEActionResponsePayload,
      PGTypes.AESession <$> genA,
      PGTypes.AEInput <$> genA
    ]

--------------------------------------------------------------------------------
-- Unexported Helpers

-- | Generator for a qualified Postgres table.
genQualifiedTable :: MonadGen m => m PGTypes.QualifiedTable
genQualifiedTable = do
  let schema = PGTypes.SchemaName <$> genIdentifier
      table = PGTypes.TableName <$> genIdentifier
  Gen.frequency
    [ (1, pure genQualifiedTableFixture),
      (9, PGTypes.QualifiedObject <$> schema <*> table)
    ]

-- | Example fixture for a Postgres 'Column'.
genQualifiedTableFixture :: PGTypes.QualifiedTable
genQualifiedTableFixture =
  let schema = PGTypes.SchemaName "schema_name"
      table = PGTypes.TableName "table_name"
   in PGTypes.QualifiedObject schema table

-- | Generator for an arbitrary Postgres identifier.
--
-- cf. https://www.postgresql.org/docs/11/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
genIdentifier :: MonadGen m => m Text
genIdentifier = do
  -- NOTE: 'Gen.alpha' is used out of convenience, but the Postgres
  -- specification states that identifiers may begin with "letters with
  -- diacritical marks and non-latin characters".
  begin <-
    Gen.frequency
      [ (9, Gen.alphaNum),
        (1, pure '_')
      ]
  -- NOTE: By default, Postgres limits identifiers to 63 bytes; since we're
  -- limiting ourselves to ASCII here that means a maximum of 62 characters.
  rest <-
    let range = Range.linearFrom 10 0 62
        gen =
          Gen.frequency
            [ (1, pure '_'),
              (1, pure '$'),
              (8, Gen.alphaNum)
            ]
     in Gen.text range gen
  -- Construct the arbitrarily generated identifier
  pure $ T.cons begin rest

genSchemaName :: MonadGen m => Range Int -> m PG.SchemaName
genSchemaName textRange =
  Gen.choice [pure PG.publicSchema, PG.SchemaName <$> genArbitraryUnicodeText textRange]

genPgFunctionName :: MonadGen m => Range Int -> m PG.FunctionName
genPgFunctionName textRange = PG.FunctionName <$> genArbitraryUnicodeText textRange

genDWithinGeomOp :: MonadGen m => m a -> m (DWithinGeomOp a)
genDWithinGeomOp genA = DWithinGeomOp <$> genA <*> genA

genDWithinGeogOp :: MonadGen m => m a -> m (DWithinGeogOp a)
genDWithinGeogOp genA = DWithinGeogOp <$> genA <*> genA <*> genA

genTIntersectsGeomminNband :: MonadGen m => m a -> m (STIntersectsGeomminNband a)
genTIntersectsGeomminNband genA = STIntersectsGeomminNband <$> genA <*> Gen.maybe genA

genSTIntersectsNbandGeommin :: MonadGen m => m a -> m (STIntersectsNbandGeommin a)
genSTIntersectsNbandGeommin genA = STIntersectsNbandGeommin <$> genA <*> genA
