{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Context
  ( RoleContext(..)
  , GQLContext(..)
  , ParserFn
  , RootField(..)
  , traverseDB
  , traverseAction
  , traverseRemoteField
  , RemoteField(..)
  , QueryDB(..)
  , ActionQuery(..)
  , QueryRootField
  , MutationDB(..)
  , ActionMutation(..)
  , MutationRootField
  , SubscriptionRootField
  , SubscriptionRootFieldResolved
  , resolveRemoteField
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.RQL.DML.Delete.Types   as RQL
import qualified Hasura.RQL.DML.Select.Types   as RQL
import qualified Hasura.RQL.DML.Update.Types   as RQL
import qualified Hasura.RQL.Types.Action       as RQL
import qualified Hasura.RQL.Types.Error        as RQL
import qualified Hasura.RQL.Types.RemoteSchema as RQL
import qualified Hasura.SQL.DML                as S

import           Hasura.SQL.Types              ((<<>))

import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Insert  (AnnInsert)

import qualified Data.Text.Read                         as T
import qualified Data.Text                              as T

import           Hasura.Session

-- | For storing both a normal GQLContext and one for the backend variant.
-- Currently, this is to enable the backend variant to have certain insert
-- permissions which the frontend variant does not.

data RoleContext a
  = RoleContext
  { _rctxDefault :: !a -- ^ The default context for normal sessions
  , _rctxBackend :: !(Maybe a) -- ^ The context for sessions with backend privilege.
  } deriving (Show, Eq, Functor, Foldable, Traversable)
$(deriveToJSON (aesonDrop 5 snakeCase) ''RoleContext)

data GQLContext = GQLContext
  { gqlQueryParser    :: ParserFn (InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  , gqlMutationParser :: Maybe (ParserFn (InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
  }

instance J.ToJSON GQLContext where
  toJSON GQLContext{} = J.String "The GraphQL schema parsers"

type ParserFn a
  =  G.SelectionSet G.NoFragments Variable
  -> Either (NESeq ParseError) (a, QueryReusability)

data RootField db remote action raw
  = RFDB db
  | RFRemote remote
  | RFAction action
  | RFRaw raw

traverseDB :: forall db db' remote action raw f
        . Applicative f
       => (db -> f db')
       -> RootField db remote action raw
       -> f (RootField db' remote action raw)
traverseDB f = \case
  RFDB x -> RFDB <$> f x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> pure $ RFAction x
  RFRaw x -> pure $ RFRaw x

traverseAction :: forall db remote action action' raw f
        . Applicative f
       => (action -> f action')
       -> RootField db remote action raw
       -> f (RootField db remote action' raw)
traverseAction f = \case
  RFDB x -> pure $ RFDB x
  RFRemote x -> pure $ RFRemote x
  RFAction x -> RFAction <$> f x
  RFRaw x -> pure $ RFRaw x

traverseRemoteField :: forall db remote remote' action raw f
        . Applicative f
       => (remote -> f remote')
       -> RootField db remote action raw
       -> f (RootField db remote' action raw)
traverseRemoteField f = \case
  RFDB x -> pure $ RFDB x
  RFRemote x -> RFRemote <$> f x
  RFAction x -> pure $ RFAction x
  RFRaw x -> pure $ RFRaw x

data QueryDB v
  = QDBSimple      (RQL.AnnSimpleSelG       v)
  | QDBPrimaryKey  (RQL.AnnSimpleSelG       v)
  | QDBAggregation (RQL.AnnAggregateSelectG v)
  | QDBConnection  (RQL.ConnectionSelect    v)

data ActionQuery v
  = AQQuery !(RQL.AnnActionExecution v)
  | AQAsync !(RQL.AnnActionAsyncQuery v)

data RemoteField
  = RemoteField
  { _rfRemoteSchemaInfo :: !RQL.RemoteSchemaInfo
  , _rfField            :: !(G.Field G.NoFragments RQL.RemoteSchemaVariable)
  } deriving (Show, Eq)

type QueryRootField v = RootField (QueryDB v) RemoteField (ActionQuery v) J.Value

data MutationDB v
  = MDBInsert (AnnInsert   v)
  | MDBUpdate (RQL.AnnUpdG v)
  | MDBDelete (RQL.AnnDelG v)

data ActionMutation v
  = AMSync !(RQL.AnnActionExecution v)
  | AMAsync !RQL.AnnActionMutationAsync

type MutationRootField v =
  RootField (MutationDB v) RemoteField (ActionMutation v) J.Value

type SubscriptionRootField v = RootField (QueryDB v) Void (RQL.AnnActionAsyncQuery v) Void
type SubscriptionRootFieldResolved = RootField (QueryDB S.SQLExp) Void RQL.AnnSimpleSel Void

resolveRemoteVariable
  :: (MonadError RQL.QErr m)
  => UserInfo
  -> RQL.RemoteSchemaVariable
  -> m Variable
resolveRemoteVariable userInfo = \case
  RQL.SessionPresetVariable sessionVar gType varName -> do
    sessionVarVal <- onNothing (getSessionVariableValue sessionVar $ _uiSession userInfo)
      $ RQL.throw400 RQL.NotFound $ sessionVar <<> " session variable expected, but not found"
    let baseType = G.getBaseType gType
        coercedValue =
          case G.unName baseType of
            "Int" -> G.VInt . fst <$> T.decimal sessionVarVal
            "Boolean" ->
              if | sessionVarVal `elem` ["true", "false"] -> Right $ G.VBoolean $ "true" == sessionVarVal
                 | otherwise -> Left $ "invalid boolean value"
            "Float" -> G.VFloat . fst <$> T.rational sessionVarVal
            "String" -> pure $ G.VString sessionVarVal
            "ID" -> pure $ G.VString sessionVarVal
            -- When we encounter a custom scalar, we just pass it as a string
            _ -> pure $ G.VString sessionVarVal
    coercedValue' <- onLeft coercedValue $
      \errMsg ->
        let errMsg' = "error while coercing " <> sessionVar <<> ": " <> T.pack errMsg
        in RQL.throw400 RQL.CoercionError errMsg'
    pure $ Variable (VIRequired varName) gType (GraphQLValue coercedValue')
  RQL.RawVariable variable -> pure variable

resolveRemoteField
  :: (MonadError RQL.QErr m)
  => UserInfo
  -> RemoteField
  -> m (RQL.RemoteSchemaInfo, G.Field G.NoFragments Variable)
resolveRemoteField userInfo (RemoteField remoteSchemaInfo remoteField) =
  traverse (resolveRemoteVariable userInfo) remoteField >>= pure . (remoteSchemaInfo,)
