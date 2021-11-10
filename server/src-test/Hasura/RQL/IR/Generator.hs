{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides generators for the IR query selection type.
--
-- The generators in this module generally take the low-level Backend-specific
-- generators as inputs and distribute them along the generators for its parts.
-- This is different from lifting the constructor because we distribute these
-- low-level generators through.
module Hasura.RQL.IR.Generator
  ( genAnnSelectG,
    genSelectFromG,
    genTablePermG,
    genSelectArgsG,
    genFields,
  )
where

import Data.Int (Int64)
import Hasura.Generator.Common
import Hasura.Prelude hiding (bool, choice, maybe, nonEmpty)
import Hasura.RQL.IR.OrderBy (OrderByItemG (OrderByItemG))
import Hasura.RQL.IR.Select
import Hasura.RQL.Types
import Hedgehog
import Hedgehog.Gen

-- | Given the required backend-specific generators, generate the IR representation
-- of a selection query.
-- The expectation is that backend-specific generator modules will specialise and
-- fully apply this function.
--
-- This generator shrinks "stringify nums" to 'False'. For more details on this
-- generator, see: 'genFieds', 'genSelectFromG', 'genTablePermG', 'genSeletArgsG'.
genAnnSelectG ::
  forall m b r f a.
  MonadGen m =>
  Eq (ScalarType b) =>
  Eq (Column b) =>
  Hashable (ScalarType b) =>
  Hashable (Column b) =>
  m (TableName b) ->
  m (Identifier b) ->
  m (FunctionName b) ->
  m (Column b) ->
  m (ScalarType b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (BasicOrderType b) ->
  m (NullsOrderType b) ->
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
  m (AnnSelectG b r f a)
genAnnSelectG
  genTableName
  genIdentifier
  genFunctionName
  genColumn
  genScalarType
  genXComputedField
  genBooleanOperators
  genBasicOrderType
  genNullsOrderType
  genA
  genFA
  fieldNameRange
  fieldsRange
  tablePermLimitRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  relInfoMappingRange
  scalarRange
  castRange
  castOpRange
  functionDefinitionListRange
  funArgPositionalRange
  funArgsNameRange
  funArgsNamedRange
  orderByRange
  limitRange
  offsetRange
  distinctRange
  opNameRange
  andRange
  orRange
  columnRange
  columnPositionRange =
    AnnSelectG
      <$> genFields genFA fieldsRange fieldNameRange
      <*> genFrom
      <*> genPerm
      <*> genArgs
      <*> bool
    where
      genFrom =
        genSelectFromG
          genTableName
          genIdentifier
          genFunctionName
          genColumn
          genScalarType
          genA
          functionDefinitionListRange
          funArgPositionalRange
          funArgsNameRange
          funArgsNamedRange
      genPerm =
        genTablePermG
          genColumn
          genTableName
          genScalarType
          genFunctionName
          genXComputedField
          genBooleanOperators
          genA
          fieldNameRange
          tablePermLimitRange
          nameRange
          hashRange
          enumRange
          valueInfoRange
          descriptionRange
          relInfoMappingRange
          scalarRange
          castRange
          castOpRange
          andRange
          orRange
          columnRange
          columnPositionRange
      genArgs =
        genSelectArgsG
          genColumn
          genTableName
          genScalarType
          genFunctionName
          genIdentifier
          genXComputedField
          genBooleanOperators
          genBasicOrderType
          genNullsOrderType
          genA
          fieldNameRange
          nameRange
          hashRange
          enumRange
          valueInfoRange
          descriptionRange
          relInfoMappingRange
          scalarRange
          castRange
          castOpRange
          orderByRange
          limitRange
          offsetRange
          distinctRange
          funArgPositionalRange
          funArgsNameRange
          funArgsNamedRange
          opNameRange
          andRange
          orRange
          columnRange
          columnPositionRange

-- | Generate a list of pairs of field names and 'a's.
--
-- | See 'genFieldName' for details on generating field names.
genFields :: MonadGen m => m a -> Range Int -> Range Int -> m (Fields a)
genFields genA fieldsRange fieldNameRange =
  list fieldsRange $ (,) <$> genFieldName fieldNameRange <*> genA

-- | Generate a FROM clause for queries.
-- The range decides how big the definition list is for selecting from functions.
--
-- See 'genFunctionArgsExpG', 'genArgumentExp' for details.
genSelectFromG ::
  forall b a m.
  MonadGen m =>
  m (TableName b) ->
  m (Identifier b) ->
  m (FunctionName b) ->
  m (Column b) ->
  m (ScalarType b) ->
  m a ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  m (SelectFromG b a)
genSelectFromG
  genTableName
  genIdentifier
  genFunctionName
  genColumn
  genScalarType
  genA
  funDefinitionListRange
  funArgPositionalRange
  funArgsNameRange
  funArgsNamedRange =
    choice [fromTable, fromIdentifier, fromFunction]
    where
      fromTable = FromTable <$> genTableName
      fromIdentifier = FromIdentifier <$> genIdentifier
      fromFunction =
        FromFunction
          <$> genFunctionName
          <*> genFunctionArgsExpG
            (genArgumentExp genIdentifier genA)
            funArgPositionalRange
            funArgsNameRange
            funArgsNamedRange
          <*> genDefList
      genDefList =
        maybe
          . list funDefinitionListRange
          $ (,) <$> genColumn <*> genScalarType

-- | Generate table permissions: filter and limit.
-- Range determines the limit used.
--
-- See 'genAnnBoolExp', 'genAnnBoolExpFld' for details.
genTablePermG ::
  MonadGen m =>
  Eq (ScalarType b) =>
  Eq (Column b) =>
  Hashable (ScalarType b) =>
  Hashable (Column b) =>
  m (Column b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b v) ->
  m v ->
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
  m (TablePermG b v)
genTablePermG
  genColumn
  genTableName
  genScalarType
  genFunctionName
  genXComputedField
  genBooleanOperators
  genV
  fieldNameRange
  limitRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  relInfoMappingRange
  scalarRange
  castRange
  castOpRange
  andRange
  orRange
  columnRange
  columnPositionRange =
    TablePerm
      <$> genAnnBoolExp
        ( genAnnBoolExpFld
            genColumn
            genTableName
            genScalarType
            genFunctionName
            genXComputedField
            genBooleanOperators
            genV
            fieldNameRange
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            relInfoMappingRange
            scalarRange
            castRange
            castOpRange
            andRange
            orRange
            columnRange
            columnPositionRange
        )
        genTableName
        andRange
        orRange
      <*> maybe (integral limitRange)

-- | Generate selection arguments (WHERE, ORDER BY, LIMIT, OFFSET, DISTINCT
-- clauses).
-- Ranges determine the number of elements we order by, the limit, offset,
-- and distinct elements.
--
-- See 'genAnnBoolExp', 'genAnnBoolExpFld', 'genAnnotatedOrderByItemG',
-- 'genAnnotatedOrderByElement' for more details.
genSelectArgsG ::
  forall b m a.
  MonadGen m =>
  Eq (ScalarType b) =>
  Eq (Column b) =>
  Hashable (ScalarType b) =>
  Hashable (Column b) =>
  m (Column b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (Identifier b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m (BasicOrderType b) ->
  m (NullsOrderType b) ->
  m a ->
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
  Range Int ->
  Range Int ->
  Range Int ->
  m (SelectArgsG b a)
genSelectArgsG
  genColumn
  genTableName
  genScalarType
  genFunctionName
  genIdentifier
  genXComputedField
  genBooleanOperators
  genBasicOrderType
  genNullsOrderType
  genA
  fieldNameRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  relInfoMappingRange
  scalarRange
  castRange
  castOpRange
  orderByRange
  limitRange
  offsetRange
  distinctRange
  funcArgPosRange
  funcArgNameRange
  funcArgNamedRange
  opNameRange
  andRange
  orRange
  columnRange
  columnPositionRange =
    SelectArgs
      <$> where'
      <*> orderBy
      <*> limit
      <*> offset
      <*> distinct
    where
      where' :: m (Maybe (AnnBoolExp b a))
      where' =
        maybe $
          genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genA
                fieldNameRange
                nameRange
                hashRange
                enumRange
                valueInfoRange
                descriptionRange
                relInfoMappingRange
                scalarRange
                castRange
                castOpRange
                andRange
                orRange
                columnRange
                columnPositionRange
            )
            genTableName
            andRange
            orRange

      orderBy :: m (Maybe (NonEmpty (AnnotatedOrderByItemG b a)))
      orderBy =
        maybe . nonEmpty orderByRange $
          genAnnotatedOrderByItemG
            genBasicOrderType
            genNullsOrderType
            ( genAnnotatedOrderByElement
                genColumn
                genTableName
                genScalarType
                genFunctionName
                genIdentifier
                genXComputedField
                genBooleanOperators
                genA
                fieldNameRange
                nameRange
                hashRange
                enumRange
                valueInfoRange
                descriptionRange
                relInfoMappingRange
                scalarRange
                castRange
                castOpRange
                funcArgPosRange
                funcArgNameRange
                funcArgNamedRange
                relInfoMappingRange
                opNameRange
                andRange
                orRange
                columnRange
                columnPositionRange
            )

      limit :: m (Maybe Int)
      limit = maybe $ integral limitRange

      offset :: m (Maybe Int64)
      offset = maybe $ integral offsetRange

      distinct :: m (Maybe (NonEmpty (Column b)))
      distinct = maybe . nonEmpty distinctRange $ genColumn

genFunctionArgsExpG ::
  MonadGen m =>
  m a ->
  Range Int ->
  Range Int ->
  Range Int ->
  m (FunctionArgsExpG a)
genFunctionArgsExpG genA positionalRange nameRange namedRange =
  FunctionArgsExp
    <$> list positionalRange genA
    <*> genHashMap (genArbitraryUnicodeText nameRange) genA namedRange

genArgumentExp :: MonadGen m => m (Identifier b) -> m a -> m (ArgumentExp b a)
genArgumentExp genIdentifier genA = choice [tableRow, session, input]
  where
    tableRow = AETableRow <$> maybe genIdentifier
    session = AESession <$> genA
    input = AEInput <$> genA

genGExists ::
  MonadGen m =>
  m a ->
  m (TableName b) ->
  Range Int ->
  Range Int ->
  m (GExists b a)
genGExists aGen tableGen andRange orRange =
  GExists <$> tableGen <*> genAnnBoolExp aGen tableGen andRange orRange

genAnnBoolExp ::
  MonadGen m =>
  m a ->
  m (TableName b) ->
  Range Int ->
  Range Int ->
  m (GBoolExp b a)
genAnnBoolExp
  aGen
  tableGen
  andRange
  orRange =
    recursive
      choice
      [boolFld]
      [boolAnd, boolOr, boolNot, boolExists]
    where
      boolAnd = BoolAnd <$> list andRange (genAnnBoolExp aGen tableGen andRange orRange)
      boolOr = BoolOr <$> list orRange (genAnnBoolExp aGen tableGen andRange orRange)
      boolNot = BoolNot <$> genAnnBoolExp aGen tableGen andRange orRange
      boolExists = BoolExists <$> genGExists aGen tableGen andRange orRange
      boolFld = BoolFld <$> aGen

genAnnBoolExpFld ::
  MonadGen m =>
  Eq (ScalarType b) =>
  Eq (Column b) =>
  Hashable (ScalarType b) =>
  Hashable (Column b) =>
  m (Column b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m a ->
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
  m (AnnBoolExpFld b a)
genAnnBoolExpFld
  genColumn
  genTableName
  genScalarType
  genFunctionName
  genXComputedField
  genBooleanOperators
  genA
  fieldNameRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  relInfoMappingRange
  scalarRange
  castRange
  castOpRange
  andRange
  orRange
  columnRange
  columnPositionRange =
    choice [column, relationship, computedField]
    where
      column =
        AVColumn
          <$> genColumnInfo
            genColumn
            genTableName
            genScalarType
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            columnPositionRange
          <*> list
            columnRange
            ( genOpExpG
                genTableName
                genColumn
                genScalarType
                genBooleanOperators
                genA
                castRange
                castOpRange
            )
      relationship =
        AVRelationship
          <$> genRelInfo genTableName genColumn nameRange relInfoMappingRange
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genA
                fieldNameRange
                nameRange
                hashRange
                enumRange
                valueInfoRange
                descriptionRange
                relInfoMappingRange
                scalarRange
                castRange
                castOpRange
                andRange
                orRange
                columnRange
                columnPositionRange
            )
            genTableName
            andRange
            orRange
      computedField =
        AVComputedField
          <$> genAnnComputedFieldBolExp
            genTableName
            genColumn
            genScalarType
            genBooleanOperators
            genXComputedField
            genFunctionName
            genA
            fieldNameRange
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            relInfoMappingRange
            scalarRange
            castRange
            castOpRange
            andRange
            orRange
            columnRange
            columnPositionRange

genRelInfo ::
  MonadGen m =>
  Eq (Column b) =>
  Hashable (Column b) =>
  m (TableName b) ->
  m (Column b) ->
  Range Int ->
  Range Int ->
  m (RelInfo b)
genRelInfo genTableName genColumn nameRange mappingRange =
  RelInfo
    <$> genRelName nameRange
    <*> genRelType
    <*> genHashMap genColumn genColumn mappingRange
    <*> genTableName
    <*> bool_
    <*> genInsertOrder

genRelName :: MonadGen m => Range Int -> m RelName
genRelName range = RelName <$> genNonEmptyText range

genRelType :: MonadGen m => m RelType
genRelType = element [ObjRel, ArrRel]

genInsertOrder :: MonadGen m => m InsertOrder
genInsertOrder = element [BeforeParent, AfterParent]

genAnnComputedFieldBolExp ::
  MonadGen m =>
  Eq (ScalarType b) =>
  Eq (Column b) =>
  Hashable (ScalarType b) =>
  Hashable (Column b) =>
  m (TableName b) ->
  m (Column b) ->
  m (ScalarType b) ->
  m (BooleanOperators b a) ->
  m (XComputedField b) ->
  m (FunctionName b) ->
  m a ->
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
  m (AnnComputedFieldBoolExp b a)
genAnnComputedFieldBolExp
  genTableName
  genColumn
  genScalarType
  genBooleanOperators
  genXComputedField
  genFunctionName
  genA
  fieldNameRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  relInfoMappingRange
  scalarRange
  castRange
  castOpRange
  andRange
  orRange
  columnRange
  columnPositionRange =
    AnnComputedFieldBoolExp
      <$> genXComputedField
      <*> genComputedFieldName fieldNameRange
      <*> genFunctionName
      <*> genSessionArgumentPresence genA
      <*> genComputedFieldBoolExp
        genTableName
        genColumn
        genScalarType
        genFunctionName
        genXComputedField
        genBooleanOperators
        genA
        fieldNameRange
        nameRange
        hashRange
        enumRange
        valueInfoRange
        descriptionRange
        relInfoMappingRange
        scalarRange
        castRange
        castOpRange
        andRange
        orRange
        columnRange
        columnPositionRange

genComputedFieldBoolExp ::
  MonadGen m =>
  Eq (ScalarType b) =>
  Eq (Column b) =>
  Hashable (ScalarType b) =>
  Hashable (Column b) =>
  m (TableName b) ->
  m (Column b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m a ->
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
  m (ComputedFieldBoolExp b a)
genComputedFieldBoolExp
  genTableName
  genColumn
  genScalarType
  genFunctionName
  genXComputedField
  genBooleanOperators
  genA
  fieldNameRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  relInfoMappingRange
  scalarRange
  castRange
  castOpRange
  andRange
  orRange
  columnRange
  columnPositionRange =
    choice
      [ CFBEScalar
          <$> list
            scalarRange
            ( genOpExpG
                genTableName
                genColumn
                genScalarType
                genBooleanOperators
                genA
                castRange
                castOpRange
            ),
        CFBETable
          <$> genTableName
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genA
                fieldNameRange
                nameRange
                hashRange
                enumRange
                valueInfoRange
                descriptionRange
                relInfoMappingRange
                scalarRange
                castRange
                castOpRange
                andRange
                orRange
                columnRange
                columnPositionRange
            )
            genTableName
            andRange
            orRange
      ]

genComputedFieldName :: MonadGen m => Range Int -> m ComputedFieldName
genComputedFieldName range = ComputedFieldName <$> genNonEmptyText range

genSessionArgumentPresence :: MonadGen m => m a -> m (SessionArgumentPresence a)
genSessionArgumentPresence genA =
  choice
    [ pure SAPNotPresent,
      SAPFirst <$> genA,
      SAPSecond <$> genA
    ]

genOpExpG ::
  MonadGen m =>
  Eq (ScalarType b) =>
  Hashable (ScalarType b) =>
  m (TableName b) ->
  m (Column b) ->
  m (ScalarType b) ->
  m (BooleanOperators b a) ->
  m a ->
  Range Int ->
  Range Int ->
  m (OpExpG b a)
genOpExpG genTableName genColumn genScalarType genBooleanOperators genA castRange castOpRange =
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
          ( list castOpRange $
              genOpExpG
                genTableName
                genColumn
                genScalarType
                genBooleanOperators
                genA
                castRange
                castOpRange
          )
          castRange
    aeq = AEQ <$> bool_ <*> genA
    ane = ANE <$> bool_ <*> genA
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

    genRootOrCurrent = element [IsRoot, IsCurrent]

genColumnType ::
  MonadGen m =>
  m (TableName b) ->
  m (ScalarType b) ->
  Range Int ->
  Range Int ->
  Range Int ->
  m (ColumnType b)
genColumnType genTableName genScalarType hashRange enumRange valueInfoRange =
  choice [columnScalar, columnEnumReference]
  where
    columnScalar = ColumnScalar <$> genScalarType
    columnEnumReference =
      ColumnEnumReference
        <$> genEnumReference genTableName hashRange enumRange valueInfoRange

genEnumReference ::
  MonadGen m =>
  m (TableName b) ->
  Range Int ->
  Range Int ->
  Range Int ->
  m (EnumReference b)
genEnumReference genTableName hashRange enumRange valueInfoRange =
  EnumReference
    <$> genTableName
    <*> genHashMap
      (genEnumValue enumRange)
      (genEnumValueInfo valueInfoRange)
      hashRange

genEnumValue :: MonadGen m => Range Int -> m EnumValue
genEnumValue range = EnumValue <$> genGName range

genEnumValueInfo :: MonadGen m => Range Int -> m EnumValueInfo
genEnumValueInfo range = EnumValueInfo <$> maybe (genArbitraryUnicodeText range)

genColumnInfo ::
  MonadGen m =>
  m (Column b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  m (ColumnInfo b)
genColumnInfo
  genColumn
  genTableName
  genScalarType
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  columnPositionRange =
    ColumnInfo
      <$> genColumn
      <*> genGName nameRange
      <*> integral columnPositionRange
      <*> genColumnType genTableName genScalarType hashRange enumRange valueInfoRange
      <*> bool_
      <*> maybe (genDescription descriptionRange)

genAnnotatedOrderByItemG ::
  MonadGen m =>
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
  MonadGen m =>
  Eq (ScalarType b) =>
  Eq (Column b) =>
  Hashable (ScalarType b) =>
  Hashable (Column b) =>
  m (Column b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  m (FunctionName b) ->
  m (Identifier b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m a ->
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
  Range Int ->
  m (AnnotatedOrderByElement b a)
genAnnotatedOrderByElement
  genColumn
  genTableName
  genScalarType
  genFunctionName
  genIdentifier
  genXComputedField
  genBooleanOperators
  genA
  fieldNameRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  mappingRange
  scalarRange
  castRange
  castOpRange
  funcArgPosRange
  funcArgNameRange
  funcArgNamedRange
  relInfoMappingRange
  opNameRange
  andRange
  orRange
  columnRange
  columnPositionRange =
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
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            columnPositionRange
      objectRelation =
        AOCObjectRelation
          <$> genRelInfo genTableName genColumn nameRange mappingRange
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genA
                fieldNameRange
                nameRange
                hashRange
                enumRange
                valueInfoRange
                descriptionRange
                mappingRange
                scalarRange
                castRange
                castOpRange
                andRange
                orRange
                columnRange
                columnPositionRange
            )
            genTableName
            andRange
            orRange
          <*> genAnnotatedOrderByElement
            genColumn
            genTableName
            genScalarType
            genFunctionName
            genIdentifier
            genXComputedField
            genBooleanOperators
            genA
            fieldNameRange
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            mappingRange
            scalarRange
            castRange
            castOpRange
            funcArgPosRange
            funcArgNameRange
            funcArgNamedRange
            relInfoMappingRange
            opNameRange
            andRange
            orRange
            columnRange
            columnPositionRange
      arrayAggregation =
        AOCArrayAggregation
          <$> genRelInfo genTableName genColumn nameRange mappingRange
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genA
                fieldNameRange
                nameRange
                hashRange
                enumRange
                valueInfoRange
                descriptionRange
                mappingRange
                scalarRange
                castRange
                castOpRange
                andRange
                orRange
                columnRange
                columnPositionRange
            )
            genTableName
            andRange
            orRange
          <*> genAnnotatedAggregateOrderBy
            genColumn
            genTableName
            genScalarType
            nameRange
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            columnPositionRange
      computedField =
        AOCComputedField
          <$> genComputedFieldOrderBy
            genColumn
            genScalarType
            genTableName
            genFunctionName
            genIdentifier
            genXComputedField
            genBooleanOperators
            genA
            fieldNameRange
            funcArgPosRange
            funcArgNameRange
            funcArgNamedRange
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            relInfoMappingRange
            scalarRange
            castRange
            castOpRange
            opNameRange
            andRange
            orRange
            columnRange
            columnPositionRange

genAnnotatedAggregateOrderBy ::
  MonadGen m =>
  m (Column b) ->
  m (TableName b) ->
  m (ScalarType b) ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  Range Int ->
  m (AnnotatedAggregateOrderBy b)
genAnnotatedAggregateOrderBy
  genColumn
  genTableName
  genScalarType
  opNameRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  columnPositionRange =
    choice
      [ pure AAOCount,
        AAOOp
          <$> genArbitraryUnicodeText opNameRange
          <*> genColumnInfo
            genColumn
            genTableName
            genScalarType
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            columnPositionRange
      ]

genComputedFieldOrderBy ::
  MonadGen m =>
  Eq (ScalarType b) =>
  Hashable (ScalarType b) =>
  Eq (Column b) =>
  Hashable (Column b) =>
  m (Column b) ->
  m (ScalarType b) ->
  m (TableName b) ->
  m (FunctionName b) ->
  m (Identifier b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m a ->
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
  m (ComputedFieldOrderBy b a)
genComputedFieldOrderBy
  genColumn
  genScalarType
  genTableName
  genFunctionName
  genIdentifier
  genXComputedField
  genBooleanOperators
  genA
  fieldNameRange
  funcArgPosRange
  funcArgNameRange
  funcArgNamedRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  relInfoMappingRange
  scalarRange
  castRange
  castOpRange
  opNameRange
  andRange
  orRange
  columnRange
  columnPositionRange =
    ComputedFieldOrderBy
      <$> genXComputedField
      <*> genComputedFieldName fieldNameRange
      <*> genFunctionName
      <*> genFunctionArgsExpG
        (genArgumentExp genIdentifier genA)
        funcArgPosRange
        funcArgNameRange
        funcArgNamedRange
      <*> genComputedFieldOrderByElement
        genColumn
        genScalarType
        genTableName
        genFunctionName
        genXComputedField
        genBooleanOperators
        genA
        fieldNameRange
        nameRange
        hashRange
        enumRange
        valueInfoRange
        descriptionRange
        relInfoMappingRange
        scalarRange
        castRange
        castOpRange
        opNameRange
        andRange
        orRange
        columnRange
        columnPositionRange

genComputedFieldOrderByElement ::
  MonadGen m =>
  Eq (ScalarType b) =>
  Hashable (ScalarType b) =>
  Eq (Column b) =>
  Hashable (Column b) =>
  m (Column b) ->
  m (ScalarType b) ->
  m (TableName b) ->
  m (FunctionName b) ->
  m (XComputedField b) ->
  m (BooleanOperators b a) ->
  m a ->
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
  m (ComputedFieldOrderByElement b a)
genComputedFieldOrderByElement
  genColumn
  genScalarType
  genTableName
  genFunctionName
  genXComputedField
  genBooleanOperators
  genA
  fieldNameRange
  nameRange
  hashRange
  enumRange
  valueInfoRange
  descriptionRange
  relInfoMappingRange
  scalarRange
  castRange
  castOpRange
  opNameRange
  andRange
  orRange
  columnRange
  columnPositionRange =
    choice
      [ CFOBEScalar <$> genScalarType,
        CFOBETableAggregation
          <$> genTableName
          <*> genAnnBoolExp
            ( genAnnBoolExpFld
                genColumn
                genTableName
                genScalarType
                genFunctionName
                genXComputedField
                genBooleanOperators
                genA
                fieldNameRange
                nameRange
                hashRange
                enumRange
                valueInfoRange
                descriptionRange
                relInfoMappingRange
                scalarRange
                castRange
                castOpRange
                andRange
                orRange
                columnRange
                columnPositionRange
            )
            genTableName
            andRange
            orRange
          <*> genAnnotatedAggregateOrderBy
            genColumn
            genTableName
            genScalarType
            opNameRange
            nameRange
            hashRange
            enumRange
            valueInfoRange
            descriptionRange
            columnPositionRange
      ]
