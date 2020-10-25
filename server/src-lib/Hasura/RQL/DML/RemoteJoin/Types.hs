module Hasura.RQL.DML.RemoteJoin.Types
  ( RemoteJoins
  , RemoteJoinMap
  , Alias
  , FieldPath(..)
  , RemoteJoin(..)
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.Extended           as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as HS
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Printer         as G
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Control.Lens
import           Data.Text.Extended                     (commaSeparated, (<<>))
import           Data.Validation

import qualified Hasura.Backends.Postgres.SQL.DML       as S
import qualified Hasura.Tracing                         as Tracing

import           Hasura.EncJSON
import           Hasura.GraphQL.Parser                  hiding (field)
import           Hasura.GraphQL.RemoteServer            (execRemoteGQ')
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.DML.Select.Types
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session


-- | A 'RemoteJoin' represents the context of remote relationship to be extracted from 'AnnFieldG's.
data RemoteJoin (b :: Backend)
  = RemoteJoin
  { _rjName          :: !FieldName -- ^ The remote join field name.
  , _rjArgs          :: ![RemoteFieldArgument] -- ^ User-provided arguments with variables.
  , _rjSelSet        :: !(G.SelectionSet G.NoFragments Variable)  -- ^ User-provided selection set of remote field.
  , _rjHasuraFields  :: !(HashSet FieldName) -- ^ Table fields.
  , _rjFieldCall     :: !(NonEmpty FieldCall) -- ^ Remote server fields.
  , _rjRemoteSchema  :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjPhantomFields :: ![ColumnInfo b]
    -- ^ Hasura fields which are not in the selection set, but are required as
    -- parameters to satisfy the remote join.
  }
deriving instance Eq (RemoteJoin 'Postgres)

-- | Path to the remote join field in query response JSON from Postgres.
newtype FieldPath = FieldPath {unFieldPath :: [FieldName]}
  deriving (Show, Eq, Semigroup, Monoid, Hashable)

type Alias = G.Name

type RemoteJoins b = NE.NonEmpty (FieldPath, NE.NonEmpty (RemoteJoin b))
type RemoteJoinMap b = Map.HashMap FieldPath (NE.NonEmpty (RemoteJoin b))
