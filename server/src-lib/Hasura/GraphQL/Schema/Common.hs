module Hasura.GraphQL.Schema.Common where

import           Hasura.Prelude

import qualified Data.Aeson                         as J
import qualified Data.HashMap.Strict                as Map
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.Text                          as T

import           Data.Either                        (isRight)
import           Data.Text.Extended
import           Language.GraphQL.Draft.Syntax      as G

import qualified Hasura.Backends.Postgres.SQL.Types as PG
import qualified Hasura.GraphQL.Execute.Types       as ET (GraphQLQueryType)
import qualified Hasura.GraphQL.Parser              as P
import qualified Hasura.RQL.IR.Select               as IR

import           Hasura.GraphQL.Parser              (UnpreparedValue)
import           Hasura.RQL.Types

type SelectExp           b = IR.AnnSimpleSelG       b (UnpreparedValue b)
type AggSelectExp        b = IR.AnnAggregateSelectG b (UnpreparedValue b)
type ConnectionSelectExp b = IR.ConnectionSelect    b (UnpreparedValue b)
type SelectArgs          b = IR.SelectArgsG         b (UnpreparedValue b)
type TablePerms          b = IR.TablePermG          b (UnpreparedValue b)
type AnnotatedFields     b = IR.AnnFieldsG          b (UnpreparedValue b)
type AnnotatedField      b = IR.AnnFieldG           b (UnpreparedValue b)

data QueryContext =
  QueryContext
  { qcStringifyNum              :: !Bool
  , qcDangerousBooleanCollapse  :: !Bool
  , qcQueryType                 :: !ET.GraphQLQueryType
  , qcRemoteRelationshipContext :: !(HashMap RemoteSchemaName (IntrospectionResult, ParsedIntrospection))
  , qcFunctionPermsContext      :: !FunctionPermissionsCtx
  }

textToName :: MonadError QErr m => Text -> m G.Name
textToName textName = G.mkName textName `onNothing` throw400 ValidationFailed
                      ("cannot include " <> textName <<> " in the GraphQL schema because "
                       <> " it is not a valid GraphQL identifier")

partialSQLExpToUnpreparedValue :: PartialSQLExp b -> P.UnpreparedValue b
partialSQLExpToUnpreparedValue (PSESessVar pftype var) = P.UVSessionVar pftype var
partialSQLExpToUnpreparedValue (PSESQLExp sqlExp)      = P.UVLiteral sqlExp

mapField
  :: Functor m
  => P.InputFieldsParser m (Maybe a)
  -> (a -> b)
  -> P.InputFieldsParser m (Maybe b)
mapField fp f = fmap (fmap f) fp

parsedSelectionsToFields
  :: (Text -> a) -- ^ how to handle @__typename@ fields
  -> OMap.InsOrdHashMap G.Name (P.ParsedSelection a)
  -> IR.Fields a
parsedSelectionsToFields mkTypename = OMap.toList
  >>> map (FieldName . G.unName *** P.handleTypename (mkTypename . G.unName))

numericAggOperators :: [G.Name]
numericAggOperators =
  [ $$(G.litName "sum")
  , $$(G.litName "avg")
  , $$(G.litName "stddev")
  , $$(G.litName "stddev_samp")
  , $$(G.litName "stddev_pop")
  , $$(G.litName "variance")
  , $$(G.litName "var_samp")
  , $$(G.litName "var_pop")
  ]

comparisonAggOperators :: [G.Name]
comparisonAggOperators = [$$(litName "max"), $$(litName "min")]

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

mkDescriptionWith :: Maybe PG.PGDescription -> Text -> G.Description
mkDescriptionWith descM defaultTxt = G.Description $ case descM of
  Nothing                         -> defaultTxt
  Just (PG.PGDescription descTxt) -> T.unlines [descTxt, "\n", defaultTxt]

-- | The default @'skip' and @'include' directives
defaultDirectives :: [P.DirectiveInfo]
defaultDirectives =
  [mkDirective $$(G.litName "skip"), mkDirective $$(G.litName "include")]
  where
    ifInputField =
      P.mkDefinition $$(G.litName "if") Nothing $ P.IFRequired $ P.TNamed $
      P.mkDefinition $$(G.litName "Boolean") Nothing P.TIScalar
    dirLocs = map G.DLExecutable
      [G.EDLFIELD, G.EDLFRAGMENT_SPREAD, G.EDLINLINE_FRAGMENT]
    mkDirective name =
      P.DirectiveInfo name Nothing [ifInputField] dirLocs

-- TODO why do we do these validations at this point? What does it mean to track
--      a function but not add it to the schema...?
--      Auke:
--        I believe the intention is simply to allow the console to do postgres data management
--      Karthikeyan: Yes, this is correct. We allowed this pre PDV but somehow
--        got removed in PDV. OTOH, I’m not sure how prevalent this feature
--        actually is
takeValidTables :: forall b. Backend b => TableCache b -> TableCache b
takeValidTables = Map.filterWithKey graphQLTableFilter . Map.filter tableFilter
  where
    tableFilter = not . isSystemDefined . _tciSystemDefined . _tiCoreInfo
    graphQLTableFilter tableName tableInfo =
      -- either the table name should be GraphQL compliant
      -- or it should have a GraphQL custom name set with it
      isRight (tableGraphQLName @b tableName) ||
      isJust (_tcCustomName $ _tciCustomConfig $ _tiCoreInfo tableInfo)

-- TODO and what about graphql-compliant function names here too?
takeValidFunctions :: forall b. FunctionCache b -> FunctionCache b
takeValidFunctions = Map.filter functionFilter
  where
    functionFilter = not . isSystemDefined . _fiSystemDefined


-- root field builder helpers

requiredFieldParser
  :: (Functor n, Functor m)
  => (a -> b)
  -> m (P.FieldParser n a)
  -> m (Maybe (P.FieldParser n b))
requiredFieldParser f = fmap $ Just . fmap f

optionalFieldParser
  :: (Functor n, Functor m)
  => (a -> b)
  -> m (Maybe (P.FieldParser n a))
  -> m (Maybe (P.FieldParser n b))
optionalFieldParser = fmap . fmap . fmap
