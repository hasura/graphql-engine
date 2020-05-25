{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( GQLContext(..)
  , ParserFn
  , QueryRootField(..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.RQL.DML.Select.Types   as RQL

import           Hasura.GraphQL.Parser

data GQLContext = GQLContext
  { gqlQueryParser :: ParserFn (HashMap G.Name (QueryRootField UnpreparedValue))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.object [] -- FIXME

type ParserFn a
  =  G.SelectionSet Variable
  -> Either (NESeq ParseError) (a, QueryReusability)

-- FIXME: taken from Resolve.hs
-- do we want to keep it the same?
data QueryRootField v
  = QRFSimple      (RQL.AnnSimpleSelG v)
  | QRFPrimaryKey  (RQL.AnnSimpleSelG v)
  | QRFAggregation (RQL.AnnAggSelG    v)
  | QRFTextValue         Text

{-

import           Hasura.Prelude

import           Data.Aeson
import           Data.Has

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types.Permission

-- | A /GraphQL context/, aka the final output of GraphQL schema generation. Used to both validate
-- incoming queries and respond to introspection queries.
--
-- Combines information from 'TyAgg', 'RootFields', and 'InsCtxMap' datatypes and adds a bit more on
-- top. Constructed via the 'mkGCtx' smart constructor.
data GCtx
  = GCtx
  -- GraphQL type information
  { _gTypes          :: !TypeMap
  , _gFields         :: !FieldMap
  , _gQueryRoot      :: !ObjTyInfo
  , _gMutRoot        :: !(Maybe ObjTyInfo)
  , _gSubRoot        :: !(Maybe ObjTyInfo)
  -- Postgres type information
  , _gOrdByCtx       :: !OrdByCtx
  , _gQueryCtxMap    :: !QueryCtxMap
  , _gMutationCtxMap :: !MutationCtxMap
  , _gInsCtxMap      :: !InsCtxMap
  } deriving (Show, Eq)

data RemoteGCtx
  = RemoteGCtx
  { _rgTypes            :: !TypeMap
  , _rgQueryRoot        :: !ObjTyInfo
  , _rgMutationRoot     :: !(Maybe ObjTyInfo)
  , _rgSubscriptionRoot :: !(Maybe ObjTyInfo)
  } deriving (Show, Eq)

instance Has TypeMap GCtx where
  getter = _gTypes
  modifier f ctx = ctx { _gTypes = f $ _gTypes ctx }

instance ToJSON GCtx where
  toJSON _ = String "ToJSON for GCtx is not implemented"

type GCtxMap = Map.HashMap RoleName GCtx

mkQueryRootTyInfo :: [ObjFldInfo] -> ObjTyInfo
mkQueryRootTyInfo flds =
  mkHsraObjTyInfo (Just "query root")
  (G.NamedType "query_root") Set.empty $
  mapFromL _fiName $ schemaFld:typeFld:flds
  where
    schemaFld = mkHsraObjFldInfo Nothing "__schema" Map.empty $
                G.toGT $ G.toNT $ G.NamedType "__Schema"
    typeFld = mkHsraObjFldInfo Nothing "__type" typeFldArgs $
              G.toGT $ G.NamedType "__Type"
    typeFldArgs = mapFromL _iviName $ pure $
      InpValInfo (Just "name of the type") "name" Nothing
      $ G.toGT $ G.toNT $ G.NamedType "String"

defaultTypes :: [TypeInfo]
defaultTypes = $(fromSchemaDocQ defaultSchema TLHasuraType)

emptyGCtx :: GCtx
emptyGCtx =
  let queryRoot = mkQueryRootTyInfo []
      allTys    = mkTyInfoMap $ TIObj queryRoot:defaultTypes
  -- for now subscription root is query root
  in GCtx allTys mempty queryRoot Nothing Nothing mempty mempty mempty mempty

-}
