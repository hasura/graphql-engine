{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "avoid Language.GraphQL.Draft.Syntax.unsafeMkName" #-}

-- | This module provides generators for the IR query selection type.
--
-- The generators in this module generally take the low-level Backend-specific
-- generators as inputs and distribute them along the generators for its parts.
-- This is different from lifting the constructor because we distribute these
-- low-level generators through.
module Hasura.RQL.IR.Generator
  ( genAnnBoolExp,
    genAnnBoolExpFld,
    genAnnRedactionExp,
    genAnnotatedOrderByItemG,
    genAnnotatedOrderByElement,
    genAnnotatedAggregateOrderBy,
    genColumnInfo,
    genFields,
    genFunctionArgsExpG,
    genIdentifier,
    genInsertOrder,
    genRelName,
    genRelType,
  )
where

import Hasura.Backends.Postgres.RQLGenerator.GenAssociatedTypes qualified as Assoc
import Hasura.Function.Cache
import Hasura.Generator.Common
import Hasura.NativeQuery.Types (NativeQueryName (..))
import Hasura.Prelude hiding (bool, choice, maybe, nonEmpty)
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Local
import Hedgehog
import Hedgehog.Gen
import Language.GraphQL.Draft.Syntax qualified as G

-- | Generate a list of pairs of field names and 'a's.
--
-- | See 'genFieldName' for details on generating field names.
genFields :: (MonadGen m) => m a -> Range Int -> Range Int -> m (Fields a)
genFields genA fieldsRange fieldNameRange =
  list fieldsRange $ (,) <$> genFieldName fieldNameRange <*> genA

genFunctionArgsExpG ::
  (MonadGen m) =>
  m a ->
  m (FunctionArgsExpG a)
genFunctionArgsExpG genA =
  FunctionArgsExp
    <$> list defaultRange genA
    <*> genHashMap (genArbitraryUnicodeText defaultRange) genA defaultRange

genAnnRedactionExp ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  (Hashable (Column b)) =>
  (Hashable (ColumnPath b)) =>
  m (Column b) ->
  m (ColumnPath b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (FunctionArgumentExp b a) ->
  m a ->
  m (AnnRedactionExp b a)
genAnnRedactionExp
  genColumn
  genColumnPath
  genTableName
  genScalarType
  genFunctionName
  genXComputedField
  genBooleanOperators
  genFunctionArgumentExp
  genA =
    recursive
      choice
      [pure NoRedaction]
      [ RedactIfFalse
          <$> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genColumnPath
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genFunctionArgumentExp
                genA
            )
            genTableName
      ]

genGExists ::
  (MonadGen m) =>
  m a ->
  m (TableName b) ->
  m (GExists b a)
genGExists aGen tableGen =
  GExists <$> tableGen <*> genAnnBoolExp aGen tableGen

genAnnBoolExp ::
  (MonadGen m) =>
  m a ->
  m (TableName b) ->
  m (GBoolExp b a)
genAnnBoolExp
  aGen
  tableGen =
    recursive
      choice
      [boolFld]
      [boolAnd, boolOr, boolNot, boolExists]
    where
      boolAnd = BoolAnd <$> list defaultRange (genAnnBoolExp aGen tableGen)
      boolOr = BoolOr <$> list defaultRange (genAnnBoolExp aGen tableGen)
      boolNot = BoolNot <$> genAnnBoolExp aGen tableGen
      boolExists = BoolExists <$> genGExists aGen tableGen
      boolFld = BoolField <$> aGen

genAnnBoolExpFld ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  (Hashable (Column b)) =>
  (Hashable (ColumnPath b)) =>
  m (Column b) ->
  m (ColumnPath b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (FunctionArgumentExp b a) ->
  m a ->
  m (AnnBoolExpFld b a)
genAnnBoolExpFld
  genColumn
  genColumnPath
  genTableName
  genScalarType
  genFunctionName
  genXComputedField
  genBooleanOperators
  genFunctionArgumentExp
  genA =
    choice [column, relationship, computedField]
    where
      column =
        AVColumn
          <$> genColumnInfo
            genColumn
            genTableName
            genScalarType
          <*> genAnnRedactionExp
            genColumn
            genColumnPath
            genTableName
            genScalarType
            genFunctionName
            genXComputedField
            genBooleanOperators
            genFunctionArgumentExp
            genA
          <*> list
            defaultRange
            ( genOpExpG
                genTableName
                genColumn
                genScalarType
                genBooleanOperators
                genA
            )
      relationship =
        AVRelationship
          <$> genRelInfo genTableName genColumnPath
          <*> ( RelationshipFilters
                  <$> genAnnBoolExp
                    ( genAnnBoolExpFld
                        genColumn
                        genColumnPath
                        genTableName
                        genScalarType
                        genFunctionName
                        genXComputedField
                        genBooleanOperators
                        genFunctionArgumentExp
                        genA
                    )
                    genTableName
                  <*> genAnnBoolExp
                    ( genAnnBoolExpFld
                        genColumn
                        genColumnPath
                        genTableName
                        genScalarType
                        genFunctionName
                        genXComputedField
                        genBooleanOperators
                        genFunctionArgumentExp
                        genA
                    )
                    genTableName
              )
      computedField =
        AVComputedField
          <$> genAnnComputedFieldBolExp
            genTableName
            genColumn
            genColumnPath
            genScalarType
            genBooleanOperators
            genXComputedField
            genFunctionName
            genFunctionArgumentExp
            genA

genRelInfo ::
  (MonadGen m) =>
  (Hashable (ColumnPath b)) =>
  m (TableName b) ->
  m (ColumnPath b) ->
  m (RelInfo b)
genRelInfo genTableName genColumn =
  RelInfo
    <$> genRelName
    <*> genRelType
    <*> (RelMapping <$> genHashMap genColumn genColumn defaultRange)
    <*> genRelTarget genTableName
    <*> bool_
    <*> genInsertOrder

genRelTarget :: (MonadGen m) => m (TableName b) -> m (RelTarget b)
genRelTarget genTableName =
  choice
    [ RelTargetTable <$> genTableName,
      RelTargetNativeQuery <$> genNativeQueryName
    ]

genNativeQueryName :: (MonadGen m) => m NativeQueryName
genNativeQueryName = NativeQueryName . G.unsafeMkName <$> Assoc.genIdentifier

genRelName :: (MonadGen m) => m RelName
genRelName = RelName <$> genNonEmptyText defaultRange

genRelType :: (MonadGen m) => m RelType
genRelType = element [ObjRel, ArrRel]

genInsertOrder :: (MonadGen m) => m InsertOrder
genInsertOrder = element [BeforeParent, AfterParent]

genAnnComputedFieldBolExp ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  (Hashable (Column b)) =>
  (Hashable (ColumnPath b)) =>
  m (TableName b) ->
  m (Column b) ->
  m (ColumnPath b) ->
  m (ScalarType b) ->
  m (BooleanOperators b a) ->
  m (XComputedField b) ->
  m (FunctionName b) ->
  m (FunctionArgumentExp b a) ->
  m a ->
  m (AnnComputedFieldBoolExp b a)
genAnnComputedFieldBolExp
  genTableName
  genColumn
  genColumnPath
  genScalarType
  genBooleanOperators
  genXComputedField
  genFunctionName
  genFunctionArgumentExp
  genA =
    AnnComputedFieldBoolExp
      <$> genXComputedField
      <*> genComputedFieldName
      <*> genFunctionName
      <*> genFunctionArgsExpG genFunctionArgumentExp
      <*> genComputedFieldBoolExp
        genTableName
        genColumn
        genColumnPath
        genScalarType
        genFunctionName
        genXComputedField
        genBooleanOperators
        genFunctionArgumentExp
        genA

genComputedFieldBoolExp ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  (Hashable (Column b)) =>
  (Hashable (ColumnPath b)) =>
  m (TableName b) ->
  m (Column b) ->
  m (ColumnPath b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (FunctionArgumentExp b a) ->
  m a ->
  m (ComputedFieldBoolExp b a)
genComputedFieldBoolExp
  genTableName
  genColumn
  genColumnPath
  genScalarType
  genFunctionName
  genXComputedField
  genBooleanOperators
  genFunctionArgumentExp
  genA =
    choice
      [ CFBEScalar
          <$> genAnnRedactionExp
            genColumn
            genColumnPath
            genTableName
            genScalarType
            genFunctionName
            genXComputedField
            genBooleanOperators
            genFunctionArgumentExp
            genA
          <*> list
            defaultRange
            ( genOpExpG
                genTableName
                genColumn
                genScalarType
                genBooleanOperators
                genA
            ),
        CFBETable
          <$> genTableName
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genColumnPath
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genFunctionArgumentExp
                genA
            )
            genTableName
      ]

genComputedFieldName :: (MonadGen m) => m ComputedFieldName
genComputedFieldName = ComputedFieldName <$> genNonEmptyText defaultRange

genOpExpG ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  m (TableName b) ->
  m (Column b) ->
  m (ScalarType b) ->
  m (BooleanOperators b a) ->
  m a ->
  m (OpExpG b a)
genOpExpG genTableName genColumn genScalarType genBooleanOperators genA =
  choice
    [ acast,
      aeq,
      ane,
      ain,
      anin,
      agt,
      alt,
      agte,
      alte,
      alike,
      anlike,
      ceq,
      cne,
      cgt,
      clt,
      cgte,
      clte,
      anIsNull,
      anIsNotNull,
      aBackendSpecific
    ]
  where
    acast =
      ACast
        <$> genHashMap
          genScalarType
          ( list defaultRange
              $ genOpExpG
                genTableName
                genColumn
                genScalarType
                genBooleanOperators
                genA
          )
          defaultRange
    aeq = AEQ <$> nullableComparison <*> genA
    ane = ANE <$> nullableComparison <*> genA
    ain = AIN <$> genA
    anin = ANIN <$> genA
    agt = AGT <$> genA
    alt = ALT <$> genA
    agte = AGTE <$> genA
    alte = ALTE <$> genA
    alike = ALIKE <$> genA
    anlike = ANLIKE <$> genA
    ceq = fmap CEQ $ RootOrCurrentColumn <$> genRootOrCurrent <*> genColumn
    cne = fmap CNE $ RootOrCurrentColumn <$> genRootOrCurrent <*> genColumn
    cgt = fmap CGT $ RootOrCurrentColumn <$> genRootOrCurrent <*> genColumn
    clt = fmap CLT $ RootOrCurrentColumn <$> genRootOrCurrent <*> genColumn
    cgte = fmap CGTE $ RootOrCurrentColumn <$> genRootOrCurrent <*> genColumn
    clte = fmap CLTE $ RootOrCurrentColumn <$> genRootOrCurrent <*> genColumn
    anIsNull = pure ANISNULL
    anIsNotNull = pure ANISNOTNULL
    aBackendSpecific = ABackendSpecific <$> genBooleanOperators

    nullableComparison =
      bool_ <&> \case
        True -> NonNullableComparison
        False -> NullableComparison

    genRootOrCurrent = element [IsRoot, IsCurrent]

genColumnType ::
  (MonadGen m) =>
  m (TableName b) ->
  m (ScalarType b) ->
  m (ColumnType b)
genColumnType genTableName genScalarType =
  choice [columnScalar, columnEnumReference]
  where
    columnScalar = ColumnScalar <$> genScalarType
    columnEnumReference =
      ColumnEnumReference
        <$> genEnumReference genTableName

genEnumReference ::
  (MonadGen m) =>
  m (TableName b) ->
  m (EnumReference b)
genEnumReference genTableName =
  EnumReference
    <$> genTableName
    <*> genHashMap
      genEnumValue
      genEnumValueInfo
      defaultRange
    <*> maybe
      ( genGName
          defaultRange
      )

genEnumValue :: (MonadGen m) => m EnumValue
genEnumValue = EnumValue <$> genGName defaultRange

genEnumValueInfo :: (MonadGen m) => m EnumValueInfo
genEnumValueInfo = EnumValueInfo <$> maybe (genArbitraryUnicodeText defaultRange)

genColumnInfo ::
  (MonadGen m) =>
  m (Column b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (ColumnInfo b)
genColumnInfo
  genColumn
  genTableName
  genScalarType =
    ColumnInfo
      <$> genColumn
      <*> genGName defaultRange
      <*> integral defaultRange
      <*> genColumnType genTableName genScalarType
      <*> bool_
      <*> maybe (genDescription defaultRange)
      <*> genColumnMutability

genColumnMutability :: (MonadGen m) => m ColumnMutability
genColumnMutability = ColumnMutability <$> bool <*> bool

genAnnotatedOrderByItemG ::
  (MonadGen m) =>
  m (BasicOrderType b) ->
  m (NullsOrderType b) ->
  m a ->
  m (OrderByItemG b a)
genAnnotatedOrderByItemG genBasicOrderType genNullsOrderType genA =
  OrderByItemG
    <$> maybe genBasicOrderType
    <*> genA
    <*> maybe genNullsOrderType

genAnnotatedOrderByElement ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  (Hashable (Column b)) =>
  (Hashable (ColumnPath b)) =>
  m (Column b) ->
  m (ColumnPath b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (FunctionArgumentExp b a) ->
  m a ->
  m (AnnotatedOrderByElement b a)
genAnnotatedOrderByElement
  genColumn
  genColumnPath
  genTableName
  genScalarType
  genFunctionName
  genXComputedField
  genBooleanOperators
  genFunctionArgumentExp
  genA =
    choice
      [ column,
        objectRelation,
        arrayAggregation,
        computedField
      ]
    where
      column =
        AOCColumn
          <$> genColumnInfo
            genColumn
            genTableName
            genScalarType
          <*> genAnnRedactionExp
            genColumn
            genColumnPath
            genTableName
            genScalarType
            genFunctionName
            genXComputedField
            genBooleanOperators
            genFunctionArgumentExp
            genA
      objectRelation =
        AOCObjectRelation
          <$> genRelInfo genTableName genColumnPath
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genColumnPath
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genFunctionArgumentExp
                genA
            )
            genTableName
          <*> genAnnotatedOrderByElement
            genColumn
            genColumnPath
            genTableName
            genScalarType
            genFunctionName
            genXComputedField
            genBooleanOperators
            genFunctionArgumentExp
            genA
      arrayAggregation =
        AOCArrayAggregation
          <$> genRelInfo genTableName genColumnPath
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genColumnPath
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genFunctionArgumentExp
                genA
            )
            genTableName
          <*> genAnnotatedAggregateOrderBy
            genColumn
            genColumnPath
            genTableName
            genScalarType
            genFunctionName
            genXComputedField
            genBooleanOperators
            genFunctionArgumentExp
            genA
      computedField =
        AOCComputedField
          <$> genComputedFieldOrderBy
            genColumn
            genColumnPath
            genScalarType
            genTableName
            genFunctionName
            genXComputedField
            genBooleanOperators
            genFunctionArgumentExp
            genA

genAnnotatedAggregateOrderBy ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  (Hashable (Column b)) =>
  (Hashable (ColumnPath b)) =>
  m (Column b) ->
  m (ColumnPath b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (FunctionArgumentExp b a) ->
  m a ->
  m (AnnotatedAggregateOrderBy b a)
genAnnotatedAggregateOrderBy
  genColumn
  genColumnPath
  genTableName
  genScalarType
  genFunctionName
  genXComputedField
  genBooleanOperators
  genFunctionArgumentExp
  genA =
    choice
      [ pure AAOCount,
        AAOOp
          <$> ( AggregateOrderByColumn
                  <$> genArbitraryUnicodeText defaultRange
                  <*> genColumnType genTableName genScalarType
                  <*> genColumnInfo
                    genColumn
                    genTableName
                    genScalarType
                  <*> genAnnRedactionExp
                    genColumn
                    genColumnPath
                    genTableName
                    genScalarType
                    genFunctionName
                    genXComputedField
                    genBooleanOperators
                    genFunctionArgumentExp
                    genA
              )
      ]

genComputedFieldOrderBy ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  (Hashable (Column b)) =>
  (Hashable (ColumnPath b)) =>
  m (Column b) ->
  m (ColumnPath b) ->
  m (ScalarType b) ->
  m (TableName b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (FunctionArgumentExp b a) ->
  m a ->
  m (ComputedFieldOrderBy b a)
genComputedFieldOrderBy
  genColumn
  genColumnPath
  genScalarType
  genTableName
  genFunctionName
  genXComputedField
  genBooleanOperators
  genFunctionArgumentExp
  genA =
    ComputedFieldOrderBy
      <$> genXComputedField
      <*> genComputedFieldName
      <*> genFunctionName
      <*> genFunctionArgsExpG genFunctionArgumentExp
      <*> genComputedFieldOrderByElement
        genColumn
        genColumnPath
        genScalarType
        genTableName
        genFunctionName
        genXComputedField
        genBooleanOperators
        genFunctionArgumentExp
        genA

genComputedFieldOrderByElement ::
  (MonadGen m) =>
  (Hashable (ScalarType b)) =>
  (Hashable (Column b)) =>
  (Hashable (ColumnPath b)) =>
  m (Column b) ->
  m (ColumnPath b) ->
  m (ScalarType b) ->
  m (TableName b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (FunctionArgumentExp b a) ->
  m a ->
  m (ComputedFieldOrderByElement b a)
genComputedFieldOrderByElement
  genColumn
  genColumnPath
  genScalarType
  genTableName
  genFunctionName
  genXComputedField
  genBooleanOperators
  genFunctionArgumentExp
  genA =
    choice
      [ CFOBEScalar
          <$> genScalarType
          <*> genAnnRedactionExp
            genColumn
            genColumnPath
            genTableName
            genScalarType
            genFunctionName
            genXComputedField
            genBooleanOperators
            genFunctionArgumentExp
            genA,
        CFOBETableAggregation
          <$> genTableName
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genColumnPath
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genFunctionArgumentExp
                genA
            )
            genTableName
          <*> genAnnotatedAggregateOrderBy
            genColumn
            genColumnPath
            genTableName
            genScalarType
            genFunctionName
            genXComputedField
            genBooleanOperators
            genFunctionArgumentExp
            genA
      ]

genIdentifier :: (MonadGen m) => m FIIdentifier
genIdentifier = Hasura.RQL.IR.Select.FIIdentifier <$> genArbitraryUnicodeText defaultRange
