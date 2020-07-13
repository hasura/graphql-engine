module Hasura.GraphQL.Resolve.Types
  ( module Hasura.GraphQL.Resolve.Types
  -- * Re-exports
  , MonadReusability(..)
  ) where

import           Control.Lens.TH
import           Hasura.Prelude

import qualified Data.Aeson                          as J
import qualified Data.HashMap.Strict                 as Map
import qualified Data.Sequence                       as Seq
import qualified Data.Sequence.NonEmpty              as NESeq
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DDL.Headers              (HeaderConf)
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML                      as S

type NodeSelectMap = Map.HashMap G.NamedType (SelOpCtx, PrimaryKeyColumns)

data QueryCtx
  = QCNodeSelect !NodeSelectMap
  | QCSelect !SelOpCtx
  | QCSelectConnection !PrimaryKeyColumns !SelOpCtx
  | QCSelectPkey !SelPkOpCtx
  | QCSelectAgg !SelOpCtx
  | QCFuncQuery !FuncQOpCtx
  | QCFuncAggQuery !FuncQOpCtx
  | QCFuncConnection !PrimaryKeyColumns !FuncQOpCtx
  | QCAsyncActionFetch !ActionSelectOpContext
  | QCAction !ActionExecutionContext
  deriving (Show, Eq)

data MutationCtx
  = MCInsert !InsOpCtx
  | MCInsertOne !InsOpCtx
  | MCUpdate !UpdOpCtx
  | MCUpdateByPk !UpdOpCtx
  | MCDelete !DelOpCtx
  | MCDeleteByPk !DelOpCtx
  | MCAction !ActionMutationExecutionContext
  deriving (Show, Eq)

type OpCtxMap a = Map.HashMap G.Name a
type QueryCtxMap = OpCtxMap QueryCtx
type MutationCtxMap = OpCtxMap MutationCtx

data InsOpCtx
  = InsOpCtx
  { _iocTable   :: !QualifiedTable
  , _iocHeaders :: ![T.Text]
  } deriving (Show, Eq)

data SelOpCtx
  = SelOpCtx
  { _socTable   :: !QualifiedTable
  , _socHeaders :: ![T.Text]
  , _socAllCols :: !PGColGNameMap
  , _socFilter  :: !AnnBoolExpPartialSQL
  , _socLimit   :: !(Maybe Int)
  } deriving (Show, Eq)

type PGColArgMap = Map.HashMap G.Name PGColumnInfo

data SelPkOpCtx
  = SelPkOpCtx
  { _spocTable   :: !QualifiedTable
  , _spocHeaders :: ![T.Text]
  , _spocFilter  :: !AnnBoolExpPartialSQL
  , _spocArgMap  :: !PGColArgMap
  } deriving (Show, Eq)

type FunctionArgSeq = Seq.Seq (InputArgument FunctionArgItem)

data FuncQOpCtx
  = FuncQOpCtx
  { _fqocFunction :: !QualifiedFunction
  , _fqocArgs     :: !FunctionArgSeq
  , _fqocHeaders  :: ![T.Text]
  , _fqocAllCols  :: !PGColGNameMap
  , _fqocFilter   :: !AnnBoolExpPartialSQL
  , _fqocLimit    :: !(Maybe Int)
  } deriving (Show, Eq)

data UpdOpCtx
  = UpdOpCtx
  { _uocTable      :: !QualifiedTable
  , _uocHeaders    :: ![T.Text]
  , _uocAllCols    :: !PGColGNameMap
  , _uocFilter     :: !AnnBoolExpPartialSQL
  , _uocCheck      :: !(Maybe AnnBoolExpPartialSQL)
  , _uocPresetCols :: !PreSetColsPartial
  } deriving (Show, Eq)

data DelOpCtx
  = DelOpCtx
  { _docTable   :: !QualifiedTable
  , _docHeaders :: ![T.Text]
  , _docAllCols :: !PGColGNameMap
  , _docFilter  :: !AnnBoolExpPartialSQL
  } deriving (Show, Eq)

data ActionExecutionContext
  = ActionExecutionContext
  { _saecName                 :: !ActionName
  , _saecOutputType           :: !GraphQLType
  , _saecOutputFields         :: !ActionOutputFields
  , _saecDefinitionList       :: ![(PGCol, PGScalarType)]
  , _saecWebhook              :: !ResolvedWebhook
  , _saecHeaders              :: ![HeaderConf]
  , _saecForwardClientHeaders :: !Bool
  } deriving (Show, Eq)

data ActionMutationExecutionContext
  = ActionMutationSyncWebhook !ActionExecutionContext
  | ActionMutationAsync
  deriving (Show, Eq)

data ActionSelectOpContext
  = ActionSelectOpContext
  { _asocOutputType     :: !GraphQLType
  , _asocDefinitionList :: ![(PGCol, PGScalarType)]
  } deriving (Show, Eq)

-- (custom name | generated name) -> PG column info
-- used in resolvers
type PGColGNameMap = Map.HashMap G.Name PGColumnInfo

data RelationshipFieldKind
  = RFKAggregate
  | RFKSimple
  | RFKConnection !PrimaryKeyColumns
  deriving (Show, Eq)

data RelationshipField
  = RelationshipField
  { _rfInfo       :: !RelInfo
  , _rfKind       :: !RelationshipFieldKind
  , _rfCols       :: !PGColGNameMap
  , _rfPermFilter :: !AnnBoolExpPartialSQL
  , _rfPermLimit  :: !(Maybe Int)
  } deriving (Show, Eq)

data ComputedFieldTable
  = ComputedFieldTable
  { _cftTable      :: !QualifiedTable
  , _cftCols       :: !PGColGNameMap
  , _cftPermFilter :: !AnnBoolExpPartialSQL
  , _cftPermLimit  :: !(Maybe Int)
  } deriving (Show, Eq)

data ComputedFieldType
  = CFTScalar !PGScalarType
  | CFTTable !ComputedFieldTable
  deriving (Show, Eq)

type ComputedFieldFunctionArgSeq = Seq.Seq FunctionArgItem

data ComputedField
  = ComputedField
  { _cfName     :: !ComputedFieldName
  , _cfFunction :: !ComputedFieldFunction
  , _cfArgSeq   :: !ComputedFieldFunctionArgSeq
  , _cfType     :: !ComputedFieldType
  } deriving (Show, Eq)

data ResolveField
  = RFPGColumn !PGColumnInfo
  | RFRelationship !RelationshipField
  | RFComputedField !ComputedField
  | RFRemoteRelationship !RemoteFieldInfo
  | RFNodeId !QualifiedTable !PrimaryKeyColumns
  deriving (Show, Eq)

type FieldMap = Map.HashMap (G.NamedType, G.Name) ResolveField

-- order by context
data OrdByItem
  = OBIPGCol !PGColumnInfo
  | OBIRel !RelInfo !AnnBoolExpPartialSQL
  | OBIAgg !RelInfo !PGColGNameMap !AnnBoolExpPartialSQL
  deriving (Show, Eq)

type OrdByItemMap = Map.HashMap G.Name OrdByItem

type OrdByCtx = Map.HashMap G.NamedType OrdByItemMap

data FunctionArgItem
  = FunctionArgItem
  { _faiInputArgName :: !G.Name
  , _faiSqlArgName   :: !(Maybe FunctionArgName)
  , _faiHasDefault   :: !HasDefault
  } deriving (Show, Eq)

-- insert context
type RelationInfoMap = Map.HashMap RelName RelInfo

data UpdPermForIns
  = UpdPermForIns
  { upfiCols   :: ![PGCol]
  , upfiCheck  :: !(Maybe AnnBoolExpPartialSQL)
  , upfiFilter :: !AnnBoolExpPartialSQL
  , upfiSet    :: !PreSetColsPartial
  } deriving (Show, Eq)

data InsCtx
  = InsCtx
  { icAllCols   :: !PGColGNameMap
  , icCheck     :: !AnnBoolExpPartialSQL
  , icSet       :: !PreSetColsPartial
  , icRelations :: !RelationInfoMap
  , icUpdPerm   :: !(Maybe UpdPermForIns)
  } deriving (Show, Eq)

type InsCtxMap = Map.HashMap QualifiedTable InsCtx

data AnnPGVal
  = AnnPGVal
  { _apvVariable   :: !(Maybe G.Variable)
  , _apvIsNullable :: !Bool
  , _apvValue      :: !(WithScalarType PGScalarValue)
  } deriving (Show, Eq)

type PrepFn m = AnnPGVal -> m S.SQLExp

-- lifts PartialSQLExp to UnresolvedVal
partialSQLExpToUnresolvedVal :: PartialSQLExp -> UnresolvedVal
partialSQLExpToUnresolvedVal = \case
  PSESessVar ty sessVar -> UVSessVar ty sessVar
  PSESQLExp s           -> UVSQL s

-- | A value that will be converted to an sql expression eventually
data UnresolvedVal
  -- | an entire session variables JSON object
  = UVSession
  | UVSessVar !(PGType PGScalarType) !SessionVariable
  -- | a SQL value literal that can be parameterized over
  | UVPG !AnnPGVal
  -- | an arbitrary SQL expression, which /cannot/ be parameterized over
  | UVSQL !S.SQLExp
  deriving (Show, Eq)

type AnnBoolExpUnresolved = AnnBoolExp UnresolvedVal

data InputFunctionArgument
  = IFAKnown !FunctionArgName !UnresolvedVal -- ^ Known value
  | IFAUnknown !FunctionArgItem -- ^ Unknown value, need to be parsed
  deriving (Show, Eq)

{- Note [Relay Node Id]
~~~~~~~~~~~~~~~~~~~~~~~

The 'Node' interface in Relay schema has exactly one field which returns
a non-null 'ID' value. Each table object type in Relay schema should implement
'Node' interface to provide global object identification.
See https://relay.dev/graphql/objectidentification.htm for more details.

To identify each row in a table, we need to encode the table information
(schema and name) and primary key column values in the 'Node' id.

Node id data:
-------------
We are using JSON format for encoding and decoding the node id. The JSON
schema looks like following

'[<version-integer>, "<table-schema>", "<table-name>", "column-1", "column-2", ... "column-n"]'

It is represented in the type @'NodeId'. The 'version-integer' represents the JSON
schema version to enable any backward compatibility if it is broken in upcoming versions.

The stringified JSON is Base64 encoded and sent to client. Also the same
base64 encoded JSON string is accepted for 'node' field resolver's 'id' input.
-}

data NodeIdVersion
  = NIVersion1
  deriving (Show, Eq)

nodeIdVersionInt :: NodeIdVersion -> Int
nodeIdVersionInt NIVersion1 = 1

currentNodeIdVersion :: NodeIdVersion
currentNodeIdVersion = NIVersion1

instance J.FromJSON NodeIdVersion where
  parseJSON v = do
    versionInt :: Int <- J.parseJSON v
    case versionInt of
      1 -> pure NIVersion1
      _ -> fail $ "expecting version 1 for node id, but got " <> show versionInt

data V1NodeId
  = V1NodeId
  { _nidTable   :: !QualifiedTable
  , _nidColumns :: !(NESeq.NESeq J.Value)
  } deriving (Show, Eq)

-- | The Relay 'Node' inteface's 'id' field value.
-- See Note [Relay Node id].
data NodeId
  = NodeIdV1 !V1NodeId
  deriving (Show, Eq)

instance J.FromJSON NodeId where
  parseJSON v = do
    valueList <- J.parseJSON v
    case valueList of
      []              -> fail "unexpected GUID format, found empty list"
      J.Number 1:rest -> NodeIdV1 <$> parseNodeIdV1 rest
      J.Number n:_    -> fail $ "unsupported GUID version: " <> show n
      _               -> fail "unexpected GUID format, needs to start with a version number"
    where
      parseNodeIdV1 (schemaValue:(nameValue:(firstColumn:remainingColumns))) =
        V1NodeId
        <$> (QualifiedObject <$> J.parseJSON schemaValue <*> J.parseJSON nameValue)
        <*> pure (NESeq.NESeq (firstColumn, Seq.fromList remainingColumns))
      parseNodeIdV1 _ = fail "GUID version 1: expecting schema name, table name and at least one column value"

-- template haskell related
$(makePrisms ''ResolveField)
$(makeLenses ''ComputedField)
$(makePrisms ''ComputedFieldType)
$(makePrisms ''InputFunctionArgument)
