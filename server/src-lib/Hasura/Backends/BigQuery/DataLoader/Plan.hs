{-# LANGUAGE DuplicateRecordFields #-}

-- | Make a plan for the data loader to execute (.Execute).

module Hasura.Backends.BigQuery.DataLoader.Plan
  ( prettyPlanned
  , runPlan
  , planSelectHeadAndTail
  , actionsForest
  , drawActionsForest
  , drawActionsForestSQL
  , Ref
  , PlannedAction(..)
  , Action(..)
  , Select(..)
  , Join(..)
  , Relationship(..)
  , FieldName(..)
  , HeadAndTail(..)
  , selectQuery
  , printDrawPlannedActions
  ) where

import           Hasura.Prelude                   hiding (head, second, tail, tell)

import qualified Data.ByteString.Char8            as S8
import qualified Data.ByteString.Lazy.Char8       as L8
import qualified Data.HashMap.Strict.InsOrd       as OMap
import qualified Data.List                        as List
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as LT
import qualified Data.Text.Lazy.Builder           as LT
import qualified Data.Text.Lazy.Encoding          as LT

import           Data.Aeson
import           Data.Bifunctor
import           Data.Graph
import           Data.Sequence                    (Seq (..))
import           Data.String
import           Data.Tree

import qualified Hasura.Backends.BigQuery.ToQuery as ToQuery
import qualified Hasura.Backends.BigQuery.Types   as BigQuery


--------------------------------------------------------------------------------
-- Types

data Ref = Ref
  { idx  :: !Int
  , text :: !Text
  } deriving (Show, Eq, Generic, Ord)
instance Hashable Ref

data PlanState = PlanState
  { actions :: !(Seq PlannedAction)
  , counter :: !Int
  }

data PlannedAction = PlannedAction
  { ref    :: Ref
  , action :: Action
  } deriving (Show)

newtype Plan a = Plan
  { unPlan :: State PlanState a
  } deriving (Functor, Applicative, Monad, MonadState PlanState)

data Action
  = SelectAction Select
  | JoinAction Join
  deriving (Show)

data Select = Select
  { selectTop          :: !BigQuery.Top
  , selectProjections  :: !(NonEmpty BigQuery.Projection)
  , selectFrom         :: !BigQuery.From
  , selectGroupBy      :: ![BigQuery.FieldName]
  , selectWhere        :: !BigQuery.Where
  , selectOrderBy      :: !(Maybe (NonEmpty BigQuery.OrderBy))
  , selectOffset       :: !(Maybe BigQuery.Expression)
  , selectRelationship :: !(Maybe Relationship)
  , selectSqlJoins     :: ![BigQuery.Join]
  , selectHaskellJoins :: ![BigQuery.Join]
  , selectAggUnwrap    :: !(Maybe Text)
  , wantedFields       :: !(Maybe [Text])
  } deriving (Show)

data Relationship = Relationship
  { leftRecordSet :: Ref
  , onFields      :: [(FieldName, FieldName)]
  , rightTable    :: BigQuery.EntityAlias
  } deriving (Show)

newtype FieldName =
  FieldName Text
  deriving (Show, Ord, Eq, Hashable, FromJSON, ToJSONKey, IsString)

data Join = Join
  { joinOn          :: [(FieldName,FieldName)]
  , leftRecordSet   :: Ref
  , rightRecordSet  :: Ref
  , joinProvenance  :: BigQuery.JoinProvenance
  , joinFieldName   :: !Text
  , joinExtractPath :: !(Maybe Text)
  , wantedFields    :: !(Maybe [Text])
  } deriving (Show)

data HeadAndTail = HeadAndTail
  { head :: Ref
  , tail :: Ref
  }

--------------------------------------------------------------------------------
-- Run planner

runPlan :: Plan r -> (r, [PlannedAction])
runPlan =
  second (toList . actions) .
  flip runState (PlanState {actions = mempty, counter = 0}) . unPlan

--------------------------------------------------------------------------------
-- Planners

planSelectHeadAndTail :: Maybe Relationship -> Maybe Text -> BigQuery.Select -> Plan HeadAndTail
planSelectHeadAndTail relationship joinExtractPath select0 = do
  ref <- generate (selectFromName (BigQuery.selectFrom select0))
  let select = fromSelect relationship joinExtractPath select0
      action = SelectAction select
  tell PlannedAction {ref, action}
  joinsFinalRef <- foldM planJoin ref (selectHaskellJoins select)
  pure
    (let head = ref
         tail = case selectHaskellJoins select of
                  [] -> ref
                  _  -> joinsFinalRef
     in HeadAndTail {head,tail})

planJoin :: Ref -> BigQuery.Join -> Plan Ref
planJoin leftRecordSet BigQuery.Join {..} = do
  ref <- generate (joinAliasName joinAlias)
  let joinFields = fmap (bimap toFieldName toFieldName) joinOn
  rightRecordSet <-
    case joinSource of
      BigQuery.JoinSelect select ->
        fmap
          (\HeadAndTail {..} -> tail)
          (planSelectHeadAndTail
             (Just
                (Relationship
                   { leftRecordSet
                   , onFields = joinFields
                   , rightTable = joinRightTable
                   }))
             joinExtractPath
             select)
  let action =
        JoinAction
          Join
            { leftRecordSet
            , rightRecordSet
            , joinOn = joinFields
            , wantedFields
            , ..
            }
  tell PlannedAction {ref, action}
  pure ref
  where
    BigQuery.JoinSelect BigQuery.Select {selectFinalWantedFields = wantedFields} =
      joinSource

--------------------------------------------------------------------------------
-- Conversions

-- TODO: Check this. We're intentionally discarding the table
-- qualification.
toFieldName :: BigQuery.FieldName -> FieldName
toFieldName (BigQuery.FieldName {fieldName = t}) = FieldName t

joinAliasName :: BigQuery.EntityAlias -> Text
joinAliasName (BigQuery.EntityAlias {entityAliasText}) = entityAliasText

selectFromName :: BigQuery.From -> Text
selectFromName (BigQuery.FromQualifiedTable (BigQuery.Aliased {aliasedThing = BigQuery.TableName {tableName}})) =
  tableName

fromSelect :: Maybe Relationship -> Maybe Text -> BigQuery.Select -> Select
fromSelect selectRelationship selectAggUnwrap BigQuery.Select {..} =
  Select
    { selectSqlJoins =
        mapMaybe
          (\case
             j@BigQuery.Join {joinProvenance = BigQuery.OrderByJoinProvenance} ->
               pure j
             j@BigQuery.Join {joinProvenance = BigQuery.ArrayAggregateJoinProvenance} ->
               pure j
             _ -> Nothing)
          selectJoins
    , selectHaskellJoins =
        mapMaybe
          (\case
             BigQuery.Join {joinProvenance = BigQuery.OrderByJoinProvenance} ->
               Nothing
             BigQuery.Join {joinProvenance = BigQuery.ArrayAggregateJoinProvenance} ->
               Nothing
             j -> pure j)
          selectJoins
    , wantedFields = selectFinalWantedFields
    , ..
    }

tell :: PlannedAction -> Plan ()
tell action = modify' (\s -> s {actions = actions s :|> action})

generate :: Text -> Plan Ref
generate text = do
  idx <- gets counter
  modify' (\s -> s {counter = counter s + 1})
  pure (Ref {idx, text})

--------------------------------------------------------------------------------
-- Plan pretty printer

prettyPlanned :: [PlannedAction] -> IO ()
prettyPlanned =
  L8.putStrLn .
  LT.encodeUtf8 . LT.toLazyText . mconcat . List.intersperse "\n\n" . map prettyPlannedAction

prettyPlannedActionsSQL :: PlannedAction -> Maybe LT.Builder
prettyPlannedActionsSQL PlannedAction {action} =
  case action of
    JoinAction {} -> Nothing
    SelectAction select -> pure query
      where (query, _params) =
              ToQuery.renderBuilderPretty
                (ToQuery.fromSelect (selectQuery select))

prettyPlannedAction :: PlannedAction -> LT.Builder
prettyPlannedAction PlannedAction {ref, action} =
  case action of
    SelectAction select ->
      mconcat
        (List.intersperse
           "\n"
           (mconcat
              (filter
                 (/= mempty)
                 [ [ "Load " <> prettyFrom (selectFrom select) <> " producing " <>
                     prettyRef ref
                   ]
                 , [ "Fields: " <>
                     mconcat
                       (List.intersperse
                          ", "
                          (map
                             prettyProjection
                             (toList (selectProjections select))))
                   ]
                 , case selectRelationship select of
                     Nothing           -> []
                     Just relationship -> [prettyRelationship relationship]
                 , (map prettyJoin (selectSqlJoins select))
                 , case selectTop select of
                     BigQuery.NoTop -> []
                     BigQuery.Top top ->
                       ["Limit " <> LT.fromText (tshow top)]
                 , ["SQL:"]
                 , [query]
                 , ["Params: " | not (null params)]
                 , map
                     (\(idx :: Int, value) ->
                        fromString (show idx) <> "=" <> fromString (show value))
                     (OMap.toList params)
                 ])))
      where (query, params) =
              ToQuery.renderBuilderPretty
                (ToQuery.fromSelect (selectQuery select))
    JoinAction Join {leftRecordSet, rightRecordSet, joinOn} ->
      mconcat
        (List.intersperse
           "\n"
           [ "Join " <> prettyRef leftRecordSet <> " with " <>
             prettyRef rightRecordSet <>
             " producing " <>
             prettyRef ref
           , "On " <> prettyJoinFields joinOn
           ])

prettyRef :: Ref -> LT.Builder
prettyRef Ref {..} = "#" <> LT.fromText (text <> tshow idx)

prettyFrom :: BigQuery.From -> LT.Builder
prettyFrom =
  \case
    BigQuery.FromQualifiedTable aliased ->
      prettyAliased
        (fmap
           (\BigQuery.TableName {tableName = t} -> (LT.fromText t))
           aliased)

prettyJoin :: BigQuery.Join -> LT.Builder
prettyJoin BigQuery.Join {..} =
  "SQL join with " <> src <> " on " <> prettyJoinFields' joinOn <> " for " <>
  reason
  where
    reason =
      case joinProvenance of
        BigQuery.OrderByJoinProvenance        -> "order by"
        BigQuery.ObjectJoinProvenance         -> "object relation"
        BigQuery.ArrayAggregateJoinProvenance -> "array aggregate relation"
        BigQuery.ArrayJoinProvenance          -> "array relation"
        BigQuery.MultiplexProvenance          -> "multiplex"
    src =
      case joinSource of
        BigQuery.JoinSelect select -> prettyFrom (BigQuery.selectFrom select)

prettyJoinFields' :: [(BigQuery.FieldName, BigQuery.FieldName)] -> LT.Builder
prettyJoinFields' onFields =
  mconcat
    (List.intersperse
       ", "
       (map
          (\(left, right) ->
             "(" <> prettyFieldName' left <> " = " <> prettyFieldName' right <>
             ")")
          onFields))

prettyRelationship :: Relationship -> LT.Builder
prettyRelationship Relationship {leftRecordSet, onFields} =
  "Relationship: " <> prettyRef leftRecordSet <> " on " <>
  prettyJoinFields onFields

prettyJoinFields :: [(FieldName, FieldName)] -> LT.Builder
prettyJoinFields onFields =
  mconcat
    (List.intersperse
       ", "
       (map
          (\(left, right) ->
             "(" <> prettyFieldName left <> " = " <> prettyFieldName right <>
             ")")
          onFields))

prettyFieldName :: FieldName -> LT.Builder
prettyFieldName (FieldName t) = LT.fromText t

prettyProjection :: BigQuery.Projection -> LT.Builder
prettyProjection =
  \case
    BigQuery.ExpressionProjection aliased ->
      prettyAliased (fmap (\_e -> "<Expression" <> ">") aliased)
    BigQuery.FieldNameProjection aliased ->
      prettyAliased (fmap prettyFieldName' aliased)
    BigQuery.AggregateProjection aliased ->
      prettyAliased (fmap (const "<Aggregate>") aliased)
    BigQuery.StarProjection -> "*"
    BigQuery.ArrayAggProjection {} -> "<ArrayAgg>"
    BigQuery.EntityProjection aliased ->
      prettyAliased (fmap (LT.fromText . joinAliasName) aliased)

prettyAliased :: BigQuery.Aliased LT.Builder -> LT.Builder
prettyAliased BigQuery.Aliased {aliasedThing, aliasedAlias} =
  aliasedThing <> " as " <> LT.fromText aliasedAlias

prettyFieldName' :: BigQuery.FieldName -> LT.Builder
prettyFieldName' (BigQuery.FieldName {fieldName, fieldNameEntity}) =
  LT.fromText (fieldNameEntity <> "." <> fieldName)

printDrawPlannedActions :: [PlannedAction] -> IO ()
printDrawPlannedActions = S8.putStrLn . T.encodeUtf8 . drawPlannedActions

drawActionsForest :: Forest PlannedAction -> Text
drawActionsForest =
  T.pack . drawForest .
  fmap (fmap (LT.unpack . LT.toLazyText . prettyPlannedAction))

drawActionsForestSQL :: Forest PlannedAction -> Text
drawActionsForestSQL =
  T.intercalate ";\n\n" .
  mapMaybe (fmap (LT.toStrict . LT.toLazyText) . prettyPlannedActionsSQL) .
  foldMap toList

drawPlannedActions :: [PlannedAction] -> Text
drawPlannedActions =
  drawActionsForest . actionsForest id

--------------------------------------------------------------------------------
-- Graphing the plan to a forest

actionsForest :: (Graph -> Graph) -> [PlannedAction] -> Forest PlannedAction
actionsForest transform actions =
  let (graph, vertex2Node, _key2Vertex) =
        graphFromEdges
          (map
             (\PlannedAction {ref, action} ->
                ( action
                , ref
                , map
                    (\PlannedAction {ref = r} -> r)
                    (filter (elem ref . plannedActionRefs) actions)))
             actions)
   in fmap
        (fmap
           ((\(action, ref, _refs) -> PlannedAction {ref, action}) . vertex2Node))
        (dff (transform graph))
  where
    plannedActionRefs PlannedAction {action} =
      case action of
        SelectAction Select {selectRelationship} ->
          case selectRelationship of
            Just Relationship {leftRecordSet} -> [leftRecordSet]
            Nothing                           -> mempty
        JoinAction Join {leftRecordSet, rightRecordSet} ->
          [leftRecordSet, rightRecordSet]

--------------------------------------------------------------------------------
-- Build a query

selectQuery :: Select -> BigQuery.Select
selectQuery Select {..} =
  BigQuery.Select
    { selectFor = BigQuery.NoFor
    , selectJoins = selectSqlJoins
    , selectFinalWantedFields = wantedFields
    , ..
    }
