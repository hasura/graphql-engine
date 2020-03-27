module Hasura.RQL.DML.Returning where

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.Text               as T
import qualified Hasura.SQL.DML          as S

data MutFldG v
  = MCount
  | MExp !T.Text
  | MRet !(AnnFldsG v)
  deriving (Show, Eq)

traverseMutFld
  :: (Applicative f)
  => (a -> f b)
  -> MutFldG a
  -> f (MutFldG b)
traverseMutFld f = \case
  MCount    -> pure MCount
  MExp t    -> pure $ MExp t
  MRet flds -> MRet <$> traverse (traverse (traverseAnnFld f)) flds

type MutFld = MutFldG S.SQLExp

type MutFldsG v = Fields (MutFldG v)

data MutationOutputG v
  = MOutMultirowFields !(MutFldsG v)
  | MOutSinglerowObject !(AnnFldsG v)
  deriving (Show, Eq)

traverseMutationOutput
  :: (Applicative f)
  => (a -> f b)
  -> MutationOutputG a -> f (MutationOutputG b)
traverseMutationOutput f = \case
  MOutMultirowFields mutationFields ->
    MOutMultirowFields <$> traverse (traverse (traverseMutFld f)) mutationFields
  MOutSinglerowObject annFields ->
    MOutSinglerowObject <$> traverseAnnFlds f annFields

type MutationOutput = MutationOutputG S.SQLExp

traverseMutFlds
  :: (Applicative f)
  => (a -> f b)
  -> MutFldsG a
  -> f (MutFldsG b)
traverseMutFlds f =
  traverse (traverse (traverseMutFld f))

type MutFlds = MutFldsG S.SQLExp

hasNestedFld :: MutationOutputG a -> Bool
hasNestedFld = \case
  MOutMultirowFields flds -> any isNestedMutFld flds
  MOutSinglerowObject annFlds -> any isNestedAnnFld annFlds
  where
    isNestedMutFld (_, mutFld) = case mutFld of
      MRet annFlds -> any isNestedAnnFld annFlds
      _            -> False
    isNestedAnnFld (_, annFld) = case annFld of
      FObj _ -> True
      FArr _ -> True
      _      -> False

pgColsFromMutFld :: MutFld -> [(PGCol, PGColumnType)]
pgColsFromMutFld = \case
  MCount -> []
  MExp _ -> []
  MRet selFlds ->
    flip mapMaybe selFlds $ \(_, annFld) -> case annFld of
    FCol (AnnColField (PGColumnInfo col _ _ colTy _ _) _ _) -> Just (col, colTy)
    _                                                       -> Nothing

pgColsFromMutFlds :: MutFlds -> [(PGCol, PGColumnType)]
pgColsFromMutFlds = concatMap (pgColsFromMutFld . snd)

pgColsToSelFlds :: [PGColumnInfo] -> [(FieldName, AnnFld)]
pgColsToSelFlds cols =
  flip map cols $
  \pgColInfo -> (fromPGCol $ pgiColumn pgColInfo, mkAnnColField pgColInfo Nothing)

mkDefaultMutFlds :: Maybe [PGColumnInfo] -> MutationOutput
mkDefaultMutFlds = MOutMultirowFields . \case
  Nothing   -> mutFlds
  Just cols -> ("returning", MRet $ pgColsToSelFlds cols):mutFlds
  where
    mutFlds = [("affected_rows", MCount)]

mkMutFldExp :: Iden -> Maybe Int -> Bool -> MutFld -> S.SQLExp
mkMutFldExp cteAlias preCalAffRows strfyNum = \case
  MCount ->
    let countExp = S.SESelect $
          S.mkSelect
          { S.selExtr = [S.Extractor S.countStar Nothing]
          , S.selFrom = Just $ S.FromExp $ pure $ S.FIIden cteAlias
          }
    in maybe countExp (S.SEUnsafe . T.pack . show) preCalAffRows
  MExp t -> S.SELit t
  MRet selFlds ->
    let tabFrom = FromIden cteAlias
        tabPerm = TablePerm annBoolExpTrue Nothing
    in S.SESelect $ mkSQLSelect JASMultipleRows $
       AnnSelG selFlds tabFrom tabPerm noTableArgs strfyNum

{- Note [Mutation output expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An example output expression for INSERT mutation:

WITH "<table-name>__mutation_result_alias" AS (
  INSERT INTO <table-name> (<insert-column>[..])
  VALUES
    (<insert-value-row>[..])
    ON CONFLICT ON CONSTRAINT "<table-constraint-name>" DO NOTHING RETURNING *,
    -- An extra column expression which performs the 'CHECK' validation
    CASE
      WHEN (<CHECK Condition>) THEN NULL
      ELSE "hdb_catalog"."check_violation"('insert check constraint failed')
    END
),
"<table-name>__all_columns_alias" AS (
  -- Only extract columns from mutated rows. Columns sorted by ordinal position so that
  -- resulted rows can be casted to table type.
  SELECT (<table-column>[..])
  FROM
    "<table-name>__mutation_result_alias"
)
<SELECT statement to generate mutation response using '<table-name>__all_columns_alias' as FROM>
-}

-- | Generate mutation output expression with given mutation CTE statement.
-- See Note [Mutation output expression].
mkMutationOutputExp
  :: QualifiedTable
  -> [PGColumnInfo]
  -> Maybe Int
  -> S.CTE
  -> MutationOutput
  -> Bool
  -> S.SelectWith
mkMutationOutputExp qt allCols preCalAffRows cte mutOutput strfyNum =
  S.SelectWith [ (S.Alias mutationResultAlias, cte)
               , (S.Alias allColumnsAlias, allColumnsSelect)
               ] sel
  where
    mutationResultAlias = Iden $ snakeCaseQualObject qt <> "__mutation_result_alias"
    allColumnsAlias = Iden $ snakeCaseQualObject qt <> "__all_columns_alias"

    allColumnsSelect = S.CTESelect $ S.mkSelect
                       { S.selExtr = map S.mkExtr $ map pgiColumn $ sortCols allCols
                       , S.selFrom = Just $ S.mkIdenFromExp mutationResultAlias
                       }

    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }
          where
            extrExp = case mutOutput of
              MOutMultirowFields mutFlds ->
                let jsonBuildObjArgs = flip concatMap mutFlds $
                      \(FieldName k, mutFld) -> [ S.SELit k
                                                , mkMutFldExp allColumnsAlias preCalAffRows strfyNum mutFld
                                                ]
                in S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

              MOutSinglerowObject annFlds ->
                let tabFrom = FromIden allColumnsAlias
                    tabPerm = TablePerm annBoolExpTrue Nothing
                in S.SESelect $ mkSQLSelect JASSingleObject $
                   AnnSelG annFlds tabFrom tabPerm noTableArgs strfyNum


checkRetCols
  :: (UserInfoM m, QErrM m)
  => FieldInfoMap FieldInfo
  -> SelPermInfo
  -> [PGCol]
  -> m [PGColumnInfo]
checkRetCols fieldInfoMap selPermInfo cols = do
  mapM_ (checkSelOnCol selPermInfo) cols
  forM cols $ \col -> askPGColInfo fieldInfoMap col relInRetErr
  where
    relInRetErr = "Relationships can't be used in \"returning\"."
