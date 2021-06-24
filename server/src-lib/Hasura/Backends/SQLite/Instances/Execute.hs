{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.SQLite.Instances.Execute where

import           Hasura.Prelude

import qualified Database.SQLite.Simple           as L
import qualified Language.GraphQL.Draft.Syntax    as G

import qualified Hasura.SQL.AnyBackend            as AB

import           Hasura.Backends.SQLite.Translate
import           Hasura.Backends.SQLite.Types
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Parser
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend
import           Hasura.Session


instance BackendExecute 'SQLite where
  type PreparedQuery    'SQLite = Text
  type MultiplexedQuery 'SQLite = ()
  type ExecutionMonad   'SQLite = IO

  mkDBQueryPlan        = slDBQueryPlan
  mkDBQueryExplain     = slDBQueryExplain
  mkDBMutationPlan     = \_ _ _ _ _ -> throw500 "not implemented!"
  mkDBSubscriptionPlan = \_ _ _ _   -> throw500 "not implemented!"
  mkLiveQueryExplain   = \_         -> throw500 "not implemented!"


slDBQueryPlan
  :: forall m.
     ( MonadError QErr m
     )
  => UserInfo
  -> SourceName
  -> SourceConfig 'SQLite
  -> QueryDB 'SQLite (Const Void) (UnpreparedValue 'SQLite)
  -> m (DBStepInfo 'SQLite)
slDBQueryPlan userInfo sourceName sourceConfig qrf = do
  query <- planInline (_uiSession userInfo) qrf
  pure $ DBStepInfo @'SQLite sourceName sourceConfig Nothing
       $ toEncJSON <$> L.query_ (slConn sourceConfig) query

toEncJSON :: [L.Only Text] -> EncJSON
toEncJSON = encJFromList . mapMaybe (fmap encJFromText . rejectEmpty . L.fromOnly)
  where
    rejectEmpty ""   = Nothing
    rejectEmpty "{}" = Nothing
    rejectEmpty x    = Just x

slDBQueryExplain
  :: MonadError QErr m
  => G.Name
  -> UserInfo
  -> SourceName
  -> SourceConfig 'SQLite
  -> QueryDB 'SQLite (Const Void) (UnpreparedValue 'SQLite)
  -> m (AB.AnyBackend DBStepInfo)
slDBQueryExplain fieldName userInfo sourceName sourceConfig qrf = do
  query <- planInline (_uiSession userInfo) qrf
  pure
    $ AB.mkAnyBackend
    $ DBStepInfo @'SQLite sourceName sourceConfig Nothing
    $ pure $ encJFromJValue
    $ ExplainPlan fieldName (Just $ L.fromQuery query) (Just ["not available"])
