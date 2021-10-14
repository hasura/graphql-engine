{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- Execute the plan given from .Plan.
module Hasura.Backends.MySQL.DataLoader.Execute where

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
import Hasura.Backends.MySQL.DataLoader.Plan hiding
  ( Join (wantedFields),
    Relationship (leftRecordSet),
    Select,
  )
import Hasura.Backends.MySQL.DataLoader.Plan qualified as DataLoaderPlan
import Hasura.Backends.MySQL.DataLoader.Plan qualified as Plan
import Hasura.Backends.MySQL.Types hiding
  ( FieldName,
    ScalarValue,
    selectWhere,
  )
import Hasura.Backends.MySQL.Types qualified as MySQL
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
  = JoinProblem ExecuteProblem
  | UnsupportedJoinBug JoinType
  | MissingRecordSetBug Ref
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
      _relationshipIn <-
        maybe (pure []) makeRelationshipIn (selectRelationship select)
      -- TODO: This record set is set to empty for now. In a follow-up
      -- code change, this will be pulled from the Connection
      -- module. However, it requires changes to the Select type,
      -- which in turn affect FromIr. In the interest of a simple PR,
      -- this is omitted. This comment will be removed when the below
      -- is updated in the follow-up PR.
      recordSet <- pure (makeRecordSet mempty)
      -- Update the wanted fields from the original select. This lets
      -- the executor know which fields to include after performing a
      -- join.
      pure recordSet {wantedFields = Plan.selectWantedFields select}
    JoinAction Plan.Join {joinType = joinType', joinFieldName = fieldName, ..} -> do
      left <- getRecordSet leftRecordSet
      right <- getRecordSet rightRecordSet
      case joinType' of
        ArrayJoin fields ->
          case leftArrayJoin
            wantedFields
            fieldName
            (toFieldNames fields)
            joinRhsTop
            joinRhsOffset
            left
            right of
            Left problem -> throwError (JoinProblem problem)
            Right recordSet -> pure recordSet
        ObjectJoin fields ->
          case leftObjectJoin
            wantedFields
            fieldName
            (toFieldNames fields)
            left
            right of
            Left problem -> throwError (JoinProblem problem)
            Right recordSet -> pure recordSet
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
  case OMap.lookup ref hash of
    Nothing -> throwError (MissingRecordSetBug ref)
    Just re -> pure re

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
            ( \row ->
                OMap.filterWithKey
                  ( \(FieldName k) _ ->
                      maybe True (elem k) (wantedFields headSet)
                  )
                  row
            )
            (rows tailSet)
      }

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

-- | Inefficient but clean left object join.
leftObjectJoin ::
  Maybe [Text] ->
  Text ->
  [(DataLoaderPlan.FieldName, DataLoaderPlan.FieldName)] ->
  RecordSet ->
  RecordSet ->
  Either ExecuteProblem RecordSet
leftObjectJoin wantedFields joinAlias joinFields left right =
  pure
    RecordSet
      { origin = Nothing,
        wantedFields = Nothing,
        rows =
          V.fromList
            [ joinObjectRows wantedFields joinAlias leftRow rightRows
              | leftRow <- toList (rows left),
                let rightRows =
                      V.fromList
                        [ rightRow
                          | rightRow <- toList (rows right),
                            not (null joinFields),
                            all
                              ( \(rightField, leftField) ->
                                  fromMaybe
                                    False
                                    ( do
                                        leftValue <-
                                          OMap.lookup leftField leftRow
                                        rightValue <-
                                          OMap.lookup rightField rightRow
                                        pure (leftValue == rightValue)
                                    )
                              )
                              joinFields
                        ]
            ]
      }

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
                                  | rightRow <- toList (rows right),
                                    not (null joinFields),
                                    all
                                      ( \(rightField, leftField) ->
                                          fromMaybe
                                            False
                                            ( do
                                                leftValue <-
                                                  OMap.lookup leftField leftRow
                                                rightValue <-
                                                  OMap.lookup rightField rightRow
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
-- We expect rightRow to consist of a single row, but don't complain
-- if this is violated. TODO: Change?
joinObjectRows ::
  Maybe [Text] ->
  Text ->
  InsOrdHashMap DataLoaderPlan.FieldName OutputValue ->
  Vector (InsOrdHashMap DataLoaderPlan.FieldName OutputValue) ->
  InsOrdHashMap DataLoaderPlan.FieldName OutputValue
joinObjectRows wantedFields fieldName leftRow rightRows =
  foldl'
    ( \left row ->
        OMap.insert
          (DataLoaderPlan.FieldName fieldName)
          ( RecordOutputValue
              ( OMap.filterWithKey
                  (\(DataLoaderPlan.FieldName k) _ -> maybe True (elem k) wantedFields)
                  row
              )
          )
          left
    )
    leftRow
    rightRows
