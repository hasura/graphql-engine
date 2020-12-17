module Hasura.GraphQL.Schema.Common where

import           Hasura.Prelude

import qualified Data.Aeson                         as J
import qualified Data.HashMap.Strict.InsOrd         as OMap

import           Data.Text.Extended
import           Language.GraphQL.Draft.Syntax      as G

import qualified Data.Text                          as T
import qualified Hasura.GraphQL.Execute.Types       as ET (GraphQLQueryType)
import qualified Hasura.GraphQL.Parser              as P
import qualified Hasura.RQL.IR.Select               as IR

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.Types
import           Hasura.GraphQL.Parser                 (UnpreparedValue)


type SelectExp           b = IR.AnnSimpleSelG       b (UnpreparedValue b)
type AggSelectExp        b = IR.AnnAggregateSelectG b (UnpreparedValue b)
type ConnectionSelectExp b = IR.ConnectionSelect    b (UnpreparedValue b)
type SelectArgs          b = IR.SelectArgsG         b (UnpreparedValue b)
type TablePerms          b = IR.TablePermG          b (UnpreparedValue b)
type AnnotatedFields     b = IR.AnnFieldsG          b (UnpreparedValue b)
type AnnotatedField      b = IR.AnnFieldG           b (UnpreparedValue b)

data QueryContext =
  QueryContext
  { qcStringifyNum :: !Bool
  , qcQueryType    :: !ET.GraphQLQueryType
  , qcRemoteFields :: !(HashMap RemoteSchemaName [P.Definition P.FieldInfo])
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

mkDescriptionWith :: Maybe PGDescription -> Text -> G.Description
mkDescriptionWith descM defaultTxt = G.Description $ case descM of
  Nothing                      -> defaultTxt
  Just (PGDescription descTxt) -> T.unlines [descTxt, "\n", defaultTxt]

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
