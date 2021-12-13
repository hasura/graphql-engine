module Hasura.RQL.IR.Postgres
  ( genTableName,
    genFunctionName,
    genColumn,
    genScalarType,
    genXComputedField,
    genBooleanOperators,
    genBasicOrderType,
    genNullsOrderType,
    genAnnSelectG,
  )
where

import Data.Int (Int64)
import Hasura.Backends.Postgres.SQL.DML
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Backends.Postgres.Types.BoolExp qualified as B
import Hasura.Generator.Common (genArbitraryUnicodeText)
import Hasura.Prelude hiding (choice, maybe)
import Hasura.RQL.IR (AnnSelectG)
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Generator qualified as IRGen
import Hasura.RQL.Types
import Hedgehog
import Hedgehog.Gen (choice, element, maybe)

genTableName :: MonadGen m => Range Int -> m (TableName ('Postgres 'Vanilla))
genTableName textRange =
  PG.QualifiedObject <$> genSchemaName textRange <*> genPgTableName textRange

genFunctionName :: MonadGen m => Range Int -> m (FunctionName ('Postgres 'Vanilla))
genFunctionName textRange =
  PG.QualifiedObject <$> genSchemaName textRange <*> genPgFunctionName textRange

genColumn :: MonadGen m => Range Int -> m (Column ('Postgres 'Vanilla))
genColumn textRange = PG.unsafePGCol <$> genArbitraryUnicodeText textRange

genScalarType :: MonadGen m => Range Int -> m (ScalarType ('Postgres 'Vanilla))
genScalarType textRange =
  choice
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
      PG.PGUnknown <$> genArbitraryUnicodeText textRange,
      PG.PGCompositeScalar <$> genArbitraryUnicodeText textRange
    ]

genBooleanOperators :: MonadGen m => m a -> m (BooleanOperators ('Postgres 'Vanilla) a)
genBooleanOperators genA =
  choice
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

genBasicOrderType :: MonadGen m => m (BasicOrderType ('Postgres 'Vanilla))
genBasicOrderType = element [OTAsc, OTDesc]

genNullsOrderType :: MonadGen m => m (NullsOrderType ('Postgres 'Vanilla))
genNullsOrderType = element [NFirst, NLast]

genXComputedField :: MonadGen m => m (XComputedField ('Postgres 'Vanilla))
genXComputedField = pure ()

genSchemaName :: MonadGen m => Range Int -> m PG.SchemaName
genSchemaName textRange =
  choice [pure PG.publicSchema, PG.SchemaName <$> genArbitraryUnicodeText textRange]

genAnnSelectG ::
  forall m r f a.
  MonadGen m =>
  Range Int ->
  m a ->
  m (f a) ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int64 ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  m (AnnSelectG ('Postgres 'Vanilla) r f a)
genAnnSelectG textRange genA =
  IRGen.genAnnSelectG
    (genTableName textRange)
    (genFunctionName textRange)
    (genColumn textRange)
    (genScalarType textRange)
    genXComputedField
    (genBooleanOperators genA)
    genBasicOrderType
    genNullsOrderType
    genA

genPgTableName :: MonadGen m => Range Int -> m PG.TableName
genPgTableName textRange = PG.TableName <$> genArbitraryUnicodeText textRange

genPgFunctionName :: MonadGen m => Range Int -> m PG.FunctionName
genPgFunctionName textRange = PG.FunctionName <$> genArbitraryUnicodeText textRange

genDWithinGeomOp :: MonadGen m => m a -> m (DWithinGeomOp a)
genDWithinGeomOp genA = DWithinGeomOp <$> genA <*> genA

genDWithinGeogOp :: MonadGen m => m a -> m (DWithinGeogOp a)
genDWithinGeogOp genA = DWithinGeogOp <$> genA <*> genA <*> genA

genTIntersectsGeomminNband :: MonadGen m => m a -> m (STIntersectsGeomminNband a)
genTIntersectsGeomminNband genA = STIntersectsGeomminNband <$> genA <*> maybe genA

genSTIntersectsNbandGeommin :: MonadGen m => m a -> m (STIntersectsNbandGeommin a)
genSTIntersectsNbandGeommin genA = STIntersectsNbandGeommin <$> genA <*> genA
