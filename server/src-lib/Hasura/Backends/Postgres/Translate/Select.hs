-- | Postgres Translate Select
--
-- This module is a translation layer between IR and postgres-specific select queries.
--
-- There are four main types of selects (as distinguished from the IR):
--
--     * "simple" selects
--
--     * aggregate selects
--
--     * connection selects (used for relay)
--
--     * streaming selects (see Hasura.Backends.Postgres.Translate.Select.Streaming for details)
module Hasura.Backends.Postgres.Translate.Select
  ( module Simple,
    module Aggregate,
    module Connection,
    module Streaming,
    PostgresTranslateSelect,
  )
where

import Hasura.Backends.Postgres.Translate.Select.Aggregate as Aggregate
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON (PostgresAnnotatedFieldJSON)
import Hasura.Backends.Postgres.Translate.Select.Connection as Connection
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (PostgresGenerateSQLSelect)
import Hasura.Backends.Postgres.Translate.Select.Simple as Simple
import Hasura.Backends.Postgres.Translate.Select.Streaming as Streaming

-- This constraint alias makes it easier to add and remove internal SQL translation constraints without
-- updating every single place this constraint is used upstream of the SQL translation logic.
type PostgresTranslateSelect pgKind = (PostgresAnnotatedFieldJSON pgKind, PostgresGenerateSQLSelect pgKind)

{- Note: [SQL generation for inherited roles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a query is executed by an inherited role, each column may contain a predicate
(AnnRedactionExp ('Postgres pgKind) SQLExp) along with it. The predicate is then
converted to a BoolExp, which will be used to check if the said column should
be nullified. For example,

Suppose there are two roles, role1 gives access only to the `addr` column with
row filter P1 and role2 gives access to both addr and phone column with row
filter P2. The `OR`ing of the predicates will have already been done while
the schema has been generated. The SQL generated will look like this:

 select
    (case when (P1 or P2) then addr else null end) as addr,
    (case when P2 then phone else null end) as phone
 from employee
 where (P1 or P2)

-}
