-- | Postgres Translate Select
--
-- This module is a translation layer between IR and postgres-specific select queries.
--
-- There are three main types of selects (as distinguished from the IR):
--
--     * "simple" selects
--
--     * aggregate selects
--
--     * connection selects (used for relay)
--
-- Most exports from this module showcase this distinction. The "interesting" parts
-- of the call tree of these functions is similar:
--
--     * 'selectQuerySQL' -> 'mkSQLSelect' -> 'processAnnSimpleSelect' -> 'processSelectParams'/'processAnnFields'
--
--     * 'selectAggregateQuerySQL' -> 'mkAggregateSelect' -> 'processAnnAggregateSelect' -> 'processSelectParams'/'processAnnFields'
--
--     * 'connetionSelectQuerySQL' -> 'mkConnectionSelect' -> 'processConnectionSelection' -> 'processSelectParams'
--
--
-- Random thoughts that might help when diving deeper in this module:
--
--     * Extractors are a pair of an SQL expression and an alias; they get
--         translated like "[SELECT ...] <expr> as <alias>"
--     * a 'SelectSource' consists of a prefix, a source, a boolean conditional
--         expression, and info on whether sorting or slicing is done
--         (needed to handle the LIMIT optimisation)
--     * For details on creating the selection tree for relationships via
--       @MonadWriter JoinTree@, see 'withWriteJoinTree'
module Hasura.Backends.Postgres.Translate.Select
  ( selectQuerySQL,
    selectStreamQuerySQL,
    selectAggregateQuerySQL,
    connectionSelectQuerySQL,
    mkSQLSelect,
    mkStreamSQLSelect,
    mkAggregateSelect,
    mkConnectionSelect,
    PostgresAnnotatedFieldJSON,
  )
where

import Hasura.Backends.Postgres.Translate.Select.Aggregate
  ( mkAggregateSelect,
    selectAggregateQuerySQL,
  )
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
  ( PostgresAnnotatedFieldJSON,
  )
import Hasura.Backends.Postgres.Translate.Select.Connection
  ( connectionSelectQuerySQL,
    mkConnectionSelect,
  )
import Hasura.Backends.Postgres.Translate.Select.Query
  ( mkSQLSelect,
    selectQuerySQL,
  )
import Hasura.Backends.Postgres.Translate.Select.Streaming
  ( mkStreamSQLSelect,
    selectStreamQuerySQL,
  )

{- Note: [SQL generation for inherited roles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a query is executed by an inherited role, each column may contain a predicate
(AnnColumnCaseBoolExp ('Postgres pgKind) SQLExp) along with it. The predicate is then
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
