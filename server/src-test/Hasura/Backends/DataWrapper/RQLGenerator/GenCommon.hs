module Hasura.Backends.DataWrapper.RQLGenerator.GenCommon
  ( genAnnBoolExpFld,
    genAnnotatedOrderByElement,

    -- * Associated Types
    genColumn,
    genTableName,
    genScalarType,
    genFunctionName,
  )
where

import Hasura.Backends.DataWrapper.IR.Name qualified as Name
import Hasura.Backends.DataWrapper.IR.Scalar.Type qualified as ScalarType
import Hasura.Generator.Common (defaultRange, genArbitraryUnicodeText, genHashMap)
import Hasura.Prelude (coerce, fmap, pure, ($), (<$>), (<*>))
import Hasura.RQL.IR
import Hasura.RQL.IR.Generator (genAnnBoolExp, genAnnotatedAggregateOrderBy, genColumnInfo, genInsertOrder, genRelName, genRelType)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Relationships.Local
import Hasura.SQL.Backend
import Hedgehog (MonadGen)
import Hedgehog.Gen (bool_, choice, element, list)

--------------------------------------------------------------------------------

genAnnBoolExpFld ::
  MonadGen m =>
  m a ->
  m (AnnBoolExpFld 'DataWrapper a)
genAnnBoolExpFld genA = choice [column, relationship]
  where
    column =
      AVColumn
        <$> genColumnInfo @_ @('DataWrapper)
          genColumn
          genTableName
          genScalarType
        <*> list defaultRange (genOpExpG genA)
    relationship =
      AVRelationship
        <$> genRelInfo
        <*> genAnnBoolExp (genAnnBoolExpFld genA) genTableName

genAnnotatedOrderByElement ::
  MonadGen m =>
  m a ->
  m (AnnotatedOrderByElement 'DataWrapper a)
genAnnotatedOrderByElement genA =
  choice
    [ column,
      objectRelation,
      arrayAggregation
    ]
  where
    column =
      AOCColumn
        <$> genColumnInfo
          genColumn
          genTableName
          genScalarType
    objectRelation =
      AOCObjectRelation
        <$> genRelInfo
        <*> genAnnBoolExp
          (genAnnBoolExpFld genA)
          genTableName
        <*> genAnnotatedOrderByElement genA
    arrayAggregation =
      AOCArrayAggregation
        <$> genRelInfo
        <*> genAnnBoolExp
          (genAnnBoolExpFld genA)
          genTableName
        <*> genAnnotatedAggregateOrderBy
          genColumn
          genTableName
          genScalarType

genColumn :: MonadGen m => m (Column 'DataWrapper)
genColumn = coerce <$> genArbitraryUnicodeText defaultRange

genTableName :: MonadGen m => m (TableName 'DataWrapper)
genTableName = coerce <$> genArbitraryUnicodeText defaultRange

genScalarType :: MonadGen m => m (ScalarType 'DataWrapper)
genScalarType =
  choice
    [ pure ScalarType.String,
      pure ScalarType.Number,
      pure ScalarType.Bool
    ]

genFunctionName :: MonadGen m => m (FunctionName 'DataWrapper)
genFunctionName = coerce <$> genArbitraryUnicodeText defaultRange

--------------------------------------------------------------------------------
-- Unexported

genOpExpG ::
  MonadGen m =>
  m a ->
  m (OpExpG 'DataWrapper a)
genOpExpG genA =
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
      anIsNotNull
    ]
  where
    acast =
      ACast
        <$> genHashMap
          genScalarType
          (list defaultRange $ genOpExpG genA)
          defaultRange
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
    genRootOrCurrent = element [IsRoot, IsCurrent]

genRelInfo ::
  MonadGen m =>
  m (RelInfo 'DataWrapper)
genRelInfo =
  RelInfo
    <$> genRelName
    <*> genRelType
    <*> genHashMap genColumn genColumn defaultRange
    <*> genTableName
    <*> bool_
    <*> genInsertOrder
