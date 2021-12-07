{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- Execute the plan given from .Plan.
module Hasura.Backends.MySQL.DataLoader.Execute
  ( OutputValue (..),
    RecordSet (..),
    ExecuteProblem (..),
    execute,
    runExecute,
    -- for testing
    joinObjectRows,
    leftObjectJoin,
  )
where

import Control.Monad.IO.Class
import Data.Aeson hiding (Value)
import Data.Aeson qualified as J
import Data.Bifunctor
import Data.Foldable
import Data.Graph
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.IORef
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.TypeLits qualified
import Hasura.Backends.MySQL.Connection (runQueryYieldingRows)
import Hasura.Backends.MySQL.DataLoader.Plan
  ( Action (..),
    FieldName (..),
    HeadAndTail (..),
    Join
      ( joinFieldName,
        joinRhsOffset,
        joinRhsTop,
        joinType,
        leftRecordSet,
        rightRecordSet
      ),
    PlannedAction (..),
    Ref,
    selectQuery,
    toFieldName,
  )
import Hasura.Backends.MySQL.DataLoader.Plan qualified as DataLoaderPlan
import Hasura.Backends.MySQL.DataLoader.Plan qualified as Plan
import Hasura.Backends.MySQL.ToQuery (fromSelect, toQueryFlat)
import Hasura.Backends.MySQL.Types hiding
  ( FieldName,
    ScalarValue,
    selectWhere,
  )
-- import Hasura.Backends.MySQL.Types qualified as MySQL
import Hasura.GraphQL.Parser ()
-- Brings an instance for Hashable (Vector a)...
import Hasura.Prelude hiding
  ( concatMap,
    elem,
    head,
    map,
    mapMaybe,
    tail,
    toList,
  )

-- | A set of records produced by the database. These are joined
-- together. There are all sorts of optimizations possible here, from
-- using a matrix/flat vector, unboxed sums for Value, etc. Presently
-- we choose a naive implementation in the interest of getting other
-- work done.
data RecordSet = RecordSet
  { origin :: !(Maybe PlannedAction),
    rows :: !(Vector (InsOrdHashMap FieldName OutputValue)),
    wantedFields :: !(Maybe [Text])
  }
  deriving (Show)

instance GHC.TypeLits.TypeError ('GHC.TypeLits.Text "Aeson loses key order, so you can't use this instance.") => ToJSON RecordSet where
  toJSON RecordSet {} = error "RecordSet.toJSON: do not use."

-- | The read-only info. used by the Execute monad. Later, this IORef
-- may become either atomically modified or in an STM or MVar so that
-- jobs can be executed in parallel.
data ExecuteReader = ExecuteReader
  { recordSets :: IORef (InsOrdHashMap Ref RecordSet),
    credentials :: !SourceConfig
  }

-- | Any problem encountered while executing the plan.
data ExecuteProblem
  = GetJobDecodeProblem String
  | CreateQueryJobDecodeProblem String
  | JoinProblem ExecuteProblem
  | UnsupportedJoinBug JoinType
  | MissingRecordSetBug Ref
  | BrokenJoinInvariant [DataLoaderPlan.FieldName]
  deriving (Show)

-- | Execute monad; as queries are performed, the record sets are
-- stored in the map.
newtype Execute a = Execute
  {unExecute :: ReaderT ExecuteReader (ExceptT ExecuteProblem IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ExecuteReader,
      MonadIO,
      MonadError ExecuteProblem
    )

-- | A value outputted by this execute module in a record set.
data OutputValue
  = ArrayOutputValue !(Vector OutputValue)
  | RecordOutputValue !(InsOrdHashMap DataLoaderPlan.FieldName OutputValue)
  | ScalarOutputValue !J.Value -- TODO: switch to 'MySQL.Scalar...'?
  | NullOutputValue
  deriving (Show, Eq, Generic)

instance Hashable OutputValue

--------------------------------------------------------------------------------
-- Main entry points

-- | Using the config, run the execute action. Finally, resolve the
-- head-and-tail to a record set.
runExecute ::
  MonadIO m =>
  SourceConfig ->
  HeadAndTail ->
  Execute a ->
  m (Either ExecuteProblem RecordSet)
runExecute credentials headAndTail action = do
  recordSets <- liftIO (newIORef mempty)
  liftIO $
    runExceptT $
      runReaderT
        (unExecute (action >> getFinalRecordSet headAndTail))
        (ExecuteReader {credentials, recordSets})

-- | Execute the forest of actions.
execute :: Forest PlannedAction -> Execute ()
execute = traverse_ (traverse_ executePlannedAction)

-- | Execute an action, then store its result in the ref assigned to it.
executePlannedAction :: PlannedAction -> Execute ()
executePlannedAction PlannedAction {ref, action} =
  fetchRecordSetForAction action >>= saveRecordSet ref

-- | Fetch the record set for the given action.
fetchRecordSetForAction :: Action -> Execute RecordSet
fetchRecordSetForAction =
  \case
    SelectAction select -> do
      recordSet <- do
        SourceConfig {scConnectionPool} <- asks credentials
        result <-
          liftIO $
            runExceptT $
              runQueryYieldingRows
                scConnectionPool
                (toQueryFlat (fromSelect (selectQuery select)))
        case result of
          Left problem -> throwError (JoinProblem problem)
          Right rows -> pure (makeRecordSet rows)
      -- Update the wanted fields from the original select. This lets
      -- the executor know which fields to include after performing a
      -- join.
      pure recordSet {wantedFields = Plan.selectWantedFields select}
    JoinAction Plan.Join {joinType = joinType', joinFieldName = fieldName, ..} -> do
      left <- getRecordSet leftRecordSet
      right <- getRecordSet rightRecordSet
      case joinType' of
        ArrayJoin fields ->
          leftArrayJoin
            wantedFields
            fieldName
            (toFieldNames fields)
            joinRhsTop
            joinRhsOffset
            left
            right
            `onLeft` (throwError . JoinProblem)
        ObjectJoin fields ->
          leftObjectJoin
            wantedFields
            fieldName
            (toFieldNames fields)
            left
            right
            `onLeft` (throwError . JoinProblem)
        _ -> throwError (UnsupportedJoinBug joinType')
  where
    toFieldNames = fmap (bimap toFieldName toFieldName)

-- | Make a record set from a flat record from the DB.
makeRecordSet :: Vector (InsOrdHashMap FieldName J.Value) -> RecordSet
makeRecordSet rows =
  RecordSet
    { origin = Nothing, -- No information for this yet, but will follow
    -- up with a change for this later.
      rows = fmap (fmap ScalarOutputValue) rows,
      wantedFields = Nothing
    }

saveRecordSet :: Ref -> RecordSet -> Execute ()
saveRecordSet ref recordSet = do
  recordSetsRef <- asks recordSets
  liftIO (modifyIORef' recordSetsRef (OMap.insert ref recordSet))

getRecordSet :: Ref -> Execute RecordSet
getRecordSet ref = do
  recordSetsRef <- asks recordSets
  hash <- liftIO (readIORef recordSetsRef)
  OMap.lookup ref hash `onNothing` throwError (MissingRecordSetBug ref)

-- | See documentation for 'HeadAndTail'.
getFinalRecordSet :: HeadAndTail -> Execute RecordSet
getFinalRecordSet HeadAndTail {..} = do
  headSet <- getRecordSet head
  tailSet <-
    if tail /= head
      then getRecordSet tail
      else pure headSet
  pure
    tailSet
      { rows =
          fmap
            ( OMap.filterWithKey
                ( \(FieldName k) _ ->
                    maybe True (elem k) (wantedFields headSet)
                )
            )
            (rows tailSet)
      }

{- WIP, it seems:
-- | Make an lhs_fk IN (rhs_fk1, rhs_fk2, ..) expression list.
makeRelationshipIn :: DataLoaderPlan.Relationship -> Execute [Expression]
makeRelationshipIn
  DataLoaderPlan.Relationship
    { leftRecordSet,
      joinType = _,
      rightTable = _rightTable
    } = do
    RecordSet {rows = _rows} <- getRecordSet leftRecordSet
    -- TODO: A follow-up PR will add IN(..) and will join on the join
    -- fields for the left/right tables. It needs support from Types.hs.
    pure []
    where
      _lookupField' k row =
        case OMap.lookup k row of
          Nothing -> Nothing
          Just x -> Just x

-- | Will be used by makeRelationshipIn for forming lhs_fk IN (rhs_fk1, rhs_fk2, ..)
planFieldNameToQueryFieldName :: EntityAlias -> FieldName -> MySQL.FieldName
planFieldNameToQueryFieldName (EntityAlias fieldNameEntity) (FieldName fieldName) =
  MySQL.FieldName {fNameEntity = fieldNameEntity, fName = fieldName}
-}

-- | Inefficient but clean left object join.
leftObjectJoin ::
  Maybe [Text] ->
  Text ->
  [(DataLoaderPlan.FieldName, DataLoaderPlan.FieldName)] ->
  RecordSet ->
  RecordSet ->
  Either ExecuteProblem RecordSet
leftObjectJoin wantedFields joinAlias joinFields left right = do
  rows' <- fmap V.fromList . traverse makeRows . toList $ rows left
  pure
    RecordSet
      { origin = Nothing,
        wantedFields = Nothing,
        rows = rows'
      }
  where
    makeRows :: InsOrdHashMap FieldName OutputValue -> Either ExecuteProblem (InsOrdHashMap FieldName OutputValue)
    makeRows leftRow =
      let rightRows =
            V.fromList
              [ rightRow
                | not (null joinFields),
                  rightRow <- toList (rows right),
                  all
                    ( \(rightField, leftField) ->
                        Just True
                          == ( do
                                 leftValue <- OMap.lookup leftField leftRow
                                 rightValue <- OMap.lookup rightField rightRow
                                 pure (leftValue == rightValue)
                             )
                    )
                    joinFields
              ]
       in -- The line below will return Left is rightRows has more than one element.
          -- Consider moving the check here if it makes sense in the future.
          joinObjectRows wantedFields joinAlias leftRow rightRows

-- | A naive, exponential reference implementation of a left join. It
-- serves as a trivial sample implementation for correctness checking
-- of more efficient ones.
leftArrayJoin ::
  Maybe [Text] ->
  Text ->
  [(DataLoaderPlan.FieldName, DataLoaderPlan.FieldName)] ->
  Top ->
  Maybe Int ->
  RecordSet ->
  RecordSet ->
  Either ExecuteProblem RecordSet
leftArrayJoin wantedFields joinAlias joinFields rhsTop rhsOffset left right =
  pure
    RecordSet
      { origin = Nothing,
        wantedFields = Nothing,
        rows =
          V.fromList
            [ joinArrayRows wantedFields joinAlias leftRow rightRows
              | leftRow <- toList (rows left),
                let rightRows =
                      V.fromList
                        ( limit
                            ( offset
                                [ rightRow
                                  | not (null joinFields),
                                    rightRow <- toList (rows right),
                                    all
                                      ( \(rightField, leftField) ->
                                          Just True
                                            == ( do
                                                   leftValue <- OMap.lookup leftField leftRow
                                                   rightValue <- OMap.lookup rightField rightRow
                                                   pure (leftValue == rightValue)
                                               )
                                      )
                                      joinFields
                                ]
                            )
                        )
            ]
      }
  where
    offset = maybe id drop rhsOffset
    limit =
      case rhsTop of
        NoTop -> id
        Top n -> take n

-- | Join a row with another as an array join.
joinArrayRows ::
  Maybe [Text] ->
  Text ->
  InsOrdHashMap DataLoaderPlan.FieldName OutputValue ->
  Vector (InsOrdHashMap DataLoaderPlan.FieldName OutputValue) ->
  InsOrdHashMap DataLoaderPlan.FieldName OutputValue
joinArrayRows wantedFields fieldName leftRow rightRow =
  OMap.insert
    (DataLoaderPlan.FieldName fieldName)
    ( ArrayOutputValue
        ( fmap
            ( RecordOutputValue
                . OMap.filterWithKey
                  ( \(DataLoaderPlan.FieldName k) _ ->
                      maybe True (elem k) wantedFields
                  )
            )
            rightRow
        )
    )
    leftRow

-- | Join a row with another as an object join.
--
-- If rightRow is not a single row, we throw 'BrokenJoinInvariant'.
joinObjectRows ::
  Maybe [Text] ->
  Text ->
  InsOrdHashMap DataLoaderPlan.FieldName OutputValue ->
  Vector (InsOrdHashMap DataLoaderPlan.FieldName OutputValue) ->
  Either ExecuteProblem (InsOrdHashMap DataLoaderPlan.FieldName OutputValue)
joinObjectRows wantedFields fieldName leftRow rightRows
  | V.length rightRows /= 1 = Left . BrokenJoinInvariant . foldMap OMap.keys $ rightRows
  | otherwise =
    let row = V.head rightRows
     in pure $
          OMap.insert
            (DataLoaderPlan.FieldName fieldName)
            ( RecordOutputValue
                ( OMap.filterWithKey
                    (\(DataLoaderPlan.FieldName k) _ -> maybe True (elem k) wantedFields)
                    row
                )
            )
            leftRow
