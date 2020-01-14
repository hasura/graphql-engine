.. meta::
   :description: Downgrade Hasura GraphQL engine version
   :keywords: hasura, docs, deployment, downgrade, version

Downgrading Hasura GraphQL engine
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


Step 1: Update Hasura GraphQL engine image version
--------------------------------------------------

The Hasura GraphQL engine runs off a Docker image and downgrades are as simple as changing the image tag to the version you want.

Based on your deployment method, follow the appropriate guide to downgrade the GraphQL engine version you're running:

- :doc:`Updating on Heroku <heroku/updating>`
- :doc:`Updating on Docker <docker/updating>`
- :doc:`Updating on Kubernetes <kubernetes/updating>`

If the GraphQL engine version you are downgrading to has a different catalogue version than your current, you will have to downgrade the catalogue
to the corresponding version manually as described below.

Step 2: Downgrade Hasura catalogue version
------------------------------------------

The Hasura GraphQL engine maintains its metadata state in a "catalogue" as described :ref:`here <hasura_metadata_schema>`.
The schema of the catalogue is versioned. Updates to the Hasura GraphQL engine may have Hasura catalogue version bumps.

During upgrades the server automatically migrates the catalogue to the latest version on startup.

Downgrades to the catalogue need to be carried out manually in case you are attempting to downgrade to a lower Hasura GraphQL engine version.

Hasura GraphQL engine versions - Catalogue version mapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. note::

  Some Hasura GraphQL engine version upgrades might bump the catalogue version by more than 1

+-----------------------------------+---------------------+
| Hasura GraphQL engine versions    | Catalogue version   |
+===================================+=====================+
| v1.0.0-beta.10                    | 27                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.8 - v1.0.0-beta.9     | 26                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.7                     | 24                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.6                     | 22                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.4 - v1.0.0-beta.5     | 19                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.2 - v1.0.0-beta.3     | 17                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.1                     | 16                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha42 - v1.0.0-alpha45   | 13                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha41                    | 12                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha40                    | 11                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha39                    | 10                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha36 - v1.0.0-alpha38   | 9                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha34 - v1.0.0-alpha35   | 7                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha31 - v1.0.0-alpha33   | 6                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha30                    | 5                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha29                    | 4                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha21 - v1.0.0-alpha28   | 3                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha16 - v1.0.0-alpha20   | 2                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha0 - v1.0.0-alpha15    | 1                   |
+-----------------------------------+---------------------+

Downgrading across catalogue versions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can downgrade the catalogue from a particular version to its previous version by executing the appropriate SQL statements provided below.

.. note::

  - Running these SQL statements while Hasura GraphQL engine is running might lead to unexpected results. It is recommended to first bring down any running
    Hasura GraphQL engine instances and then run the SQL statements using an external Postgres client (like ``psql``).

  - Catalogue version downgrades need to be done sequentially.

    i.e. if you need to downgrade the catalogue version from, say, version 10 to version 8, you need to first run the SQL statements to
    downgrade from version 10 to version 9 and then the ones for version 9 to version 8

.. contents:: Downgrading
  :backlinks: none
  :depth: 1
  :local:

From 27 to 26
"""""""""""""
.. code-block:: plpgsql

   ALTER TABLE hdb_catalog.event_log DROP COLUMN archived;
   DROP INDEX event_log_delivered_idx;

   UPDATE hdb_catalog.hdb_version
      SET version = '26'
    WHERE version = '27';

From 26 to 25
"""""""""""""
.. code-block:: plpgsql

   DROP VIEW hdb_catalog.hdb_function_info_agg;
   DROP VIEW hdb_catalog.hdb_function_agg;

   CREATE VIEW hdb_catalog.hdb_function_agg AS
   (
   SELECT
     p.proname::text AS function_name,
     pn.nspname::text AS function_schema,
     pd.description,

     CASE
       WHEN (p.provariadic = (0) :: oid) THEN false
       ELSE true
     END AS has_variadic,

     CASE
       WHEN (
         (p.provolatile) :: text = ('i' :: character(1)) :: text
       ) THEN 'IMMUTABLE' :: text
       WHEN (
         (p.provolatile) :: text = ('s' :: character(1)) :: text
       ) THEN 'STABLE' :: text
       WHEN (
         (p.provolatile) :: text = ('v' :: character(1)) :: text
       ) THEN 'VOLATILE' :: text
       ELSE NULL :: text
     END AS function_type,

     pg_get_functiondef(p.oid) AS function_definition,

     rtn.nspname::text AS return_type_schema,
     rt.typname::text AS return_type_name,

     CASE
       WHEN ((rt.typtype) :: text = ('b' :: character(1)) :: text) THEN 'BASE' :: text
       WHEN ((rt.typtype) :: text = ('c' :: character(1)) :: text) THEN 'COMPOSITE' :: text
       WHEN ((rt.typtype) :: text = ('d' :: character(1)) :: text) THEN 'DOMAIN' :: text
       WHEN ((rt.typtype) :: text = ('e' :: character(1)) :: text) THEN 'ENUM' :: text
       WHEN ((rt.typtype) :: text = ('r' :: character(1)) :: text) THEN 'RANGE' :: text
       WHEN ((rt.typtype) :: text = ('p' :: character(1)) :: text) THEN 'PSEUDO' :: text
       ELSE NULL :: text
     END AS return_type_type,
     p.proretset AS returns_set,
     ( SELECT
         COALESCE(json_agg(q.type_name), '[]')
       FROM
         (
           SELECT
             pt.typname AS type_name,
             pat.ordinality
           FROM
             unnest(
               COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
             ) WITH ORDINALITY pat(oid, ordinality)
             LEFT JOIN pg_type pt ON ((pt.oid = pat.oid))
           ORDER BY pat.ordinality ASC
         ) q
      ) AS input_arg_types,
     to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names,
     p.pronargdefaults AS default_args
   FROM
     pg_proc p
     JOIN pg_namespace pn ON (pn.oid = p.pronamespace)
     JOIN pg_type rt ON (rt.oid = p.prorettype)
     JOIN pg_namespace rtn ON (rtn.oid = rt.typnamespace)
     LEFT JOIN pg_description pd ON p.oid = pd.objoid
   WHERE
     pn.nspname :: text NOT LIKE 'pg_%'
     AND pn.nspname :: text NOT IN ('information_schema', 'hdb_catalog', 'hdb_views')
     AND (NOT EXISTS (
             SELECT
               1
             FROM
               pg_aggregate
             WHERE
               ((pg_aggregate.aggfnoid) :: oid = p.oid)
           )
       )
   );

   CREATE VIEW hdb_catalog.hdb_function_info_agg AS (
     SELECT
       function_name,
       function_schema,
       row_to_json (
         (
           SELECT
             e
             FROM
                 (
                   SELECT
                     description,
                     has_variadic,
                     function_type,
                     return_type_schema,
                     return_type_name,
                     return_type_type,
                     returns_set,
                     input_arg_types,
                     input_arg_names,
                     default_args,
                     exists(
                       SELECT
                         1
                         FROM
                             information_schema.tables
                        WHERE
                   table_schema = return_type_schema
               AND table_name = return_type_name
                     ) AS returns_table
                 ) AS e
         )
       ) AS "function_info"
       FROM
           hdb_catalog.hdb_function_agg
   );

   DROP VIEW hdb_catalog.hdb_computed_field_function;

   DROP TABLE hdb_catalog.hdb_computed_field;

   UPDATE hdb_catalog.hdb_version
      SET version = '25'
    WHERE version = '26';

From 25 to 24
"""""""""""""
.. code-block:: plpgsql

  CREATE OR REPLACE VIEW hdb_catalog.hdb_column AS
       WITH primary_key_references AS (
              SELECT fkey.table_schema           AS src_table_schema
                   , fkey.table_name             AS src_table_name
                   , fkey.columns->>0            AS src_column_name
                   , json_agg(json_build_object(
                       'schema', fkey.ref_table_table_schema,
                       'name', fkey.ref_table
                     )) AS ref_tables
                FROM hdb_catalog.hdb_foreign_key_constraint AS fkey
                JOIN hdb_catalog.hdb_primary_key            AS pkey
                      ON pkey.table_schema   = fkey.ref_table_table_schema
                     AND pkey.table_name     = fkey.ref_table
                     AND pkey.columns::jsonb = fkey.ref_columns::jsonb
               WHERE json_array_length(fkey.columns) = 1
            GROUP BY fkey.table_schema
                   , fkey.table_name
                   , fkey.columns->>0)
     SELECT columns.table_schema
          , columns.table_name
          , columns.column_name AS name
          , columns.udt_name AS type
          , columns.is_nullable
          , columns.ordinal_position
          , coalesce(pkey_refs.ref_tables, '[]') AS primary_key_references
          , col_description(pg_class.oid, columns.ordinal_position) AS description
       FROM information_schema.columns
  LEFT JOIN primary_key_references AS pkey_refs
             ON columns.table_schema = pkey_refs.src_table_schema
            AND columns.table_name   = pkey_refs.src_table_name
            AND columns.column_name  = pkey_refs.src_column_name
  LEFT JOIN pg_class ON pg_class.relname = columns.table_name
  LEFT JOIN pg_namespace ON pg_namespace.oid = pg_class.relnamespace
            AND pg_namespace.nspname = columns.table_schema;

  UPDATE hdb_catalog.hdb_version
     SET version = '24'
   WHERE version = '25';

From 24 to 23
"""""""""""""
.. code-block:: plpgsql

  ALTER TABLE hdb_catalog.hdb_table DROP COLUMN configuration;

  UPDATE hdb_catalog.hdb_version
     SET version = '23'
   WHERE version = '24';

From 23 to 22
"""""""""""""

.. code-block:: plpgsql

  DROP VIEW IF EXISTS hdb_catalog.hdb_table_info_agg;
  DROP VIEW IF EXISTS hdb_catalog.hdb_column;

  CREATE VIEW hdb_catalog.hdb_column AS
       WITH primary_key_references AS (
              SELECT fkey.table_schema           AS src_table_schema
                   , fkey.table_name             AS src_table_name
                   , fkey.columns->>0            AS src_column_name
                   , json_agg(json_build_object(
                       'schema', fkey.ref_table_table_schema,
                       'name', fkey.ref_table
                     )) AS ref_tables
                FROM hdb_catalog.hdb_foreign_key_constraint AS fkey
                JOIN hdb_catalog.hdb_primary_key            AS pkey
                      ON pkey.table_schema   = fkey.ref_table_table_schema
                     AND pkey.table_name     = fkey.ref_table
                     AND pkey.columns::jsonb = fkey.ref_columns::jsonb
               WHERE json_array_length(fkey.columns) = 1
            GROUP BY fkey.table_schema
                   , fkey.table_name
                   , fkey.columns->>0)
     SELECT columns.table_schema
          , columns.table_name
          , columns.column_name AS name
          , columns.udt_name AS type
          , columns.is_nullable
          , columns.ordinal_position
          , coalesce(pkey_refs.ref_tables, '[]') AS primary_key_references
       FROM information_schema.columns
  LEFT JOIN primary_key_references AS pkey_refs
             ON columns.table_schema = pkey_refs.src_table_schema
            AND columns.table_name   = pkey_refs.src_table_name
            AND columns.column_name  = pkey_refs.src_column_name;

  CREATE VIEW hdb_catalog.hdb_table_info_agg AS (
  select
    tables.table_name as table_name,
    tables.table_schema as table_schema,
    coalesce(columns.columns, '[]') as columns,
    coalesce(pk.columns, '[]') as primary_key_columns,
    coalesce(constraints.constraints, '[]') as constraints,
    coalesce(views.view_info, 'null') as view_info
  from
    information_schema.tables as tables
    left outer join (
      select
        c.table_name,
        c.table_schema,
        json_agg(
          json_build_object(
            'name', name,
            'type', type,
            'is_nullable', is_nullable :: boolean,
            'references', primary_key_references
          )
        ) as columns
      from
        hdb_catalog.hdb_column c
      group by
        c.table_schema,
        c.table_name
    ) columns on (
      tables.table_schema = columns.table_schema
      AND tables.table_name = columns.table_name
    )
    left outer join (
      select * from hdb_catalog.hdb_primary_key
    ) pk on (
      tables.table_schema = pk.table_schema
      AND tables.table_name = pk.table_name
    )
    left outer join (
      select
        c.table_schema,
        c.table_name,
        json_agg(constraint_name) as constraints
      from
        information_schema.table_constraints c
      where
        c.constraint_type = 'UNIQUE'
        or c.constraint_type = 'PRIMARY KEY'
      group by
        c.table_schema,
        c.table_name
    ) constraints on (
      tables.table_schema = constraints.table_schema
      AND tables.table_name = constraints.table_name
    )
    left outer join (
      select
        table_schema,
        table_name,
        json_build_object(
          'is_updatable',
          (is_updatable::boolean OR is_trigger_updatable::boolean),
          'is_deletable',
          (is_updatable::boolean OR is_trigger_deletable::boolean),
          'is_insertable',
          (is_insertable_into::boolean OR is_trigger_insertable_into::boolean)
        ) as view_info
      from
        information_schema.views v
    ) views on (
      tables.table_schema = views.table_schema
      AND tables.table_name = views.table_name
    )
  );

  DROP VIEW IF EXISTS hdb_catalog.hdb_function_info_agg;
  DROP VIEW IF EXISTS hdb_catalog.hdb_function_agg;

  CREATE VIEW hdb_catalog.hdb_function_agg AS
  (
  SELECT
    p.proname::text AS function_name,
    pn.nspname::text AS function_schema,

    CASE
      WHEN (p.provariadic = (0) :: oid) THEN false
      ELSE true
    END AS has_variadic,

    CASE
      WHEN (
        (p.provolatile) :: text = ('i' :: character(1)) :: text
      ) THEN 'IMMUTABLE' :: text
      WHEN (
        (p.provolatile) :: text = ('s' :: character(1)) :: text
      ) THEN 'STABLE' :: text
      WHEN (
        (p.provolatile) :: text = ('v' :: character(1)) :: text
      ) THEN 'VOLATILE' :: text
      ELSE NULL :: text
    END AS function_type,

    pg_get_functiondef(p.oid) AS function_definition,

    rtn.nspname::text AS return_type_schema,
    rt.typname::text AS return_type_name,

    CASE
      WHEN ((rt.typtype) :: text = ('b' :: character(1)) :: text) THEN 'BASE' :: text
      WHEN ((rt.typtype) :: text = ('c' :: character(1)) :: text) THEN 'COMPOSITE' :: text
      WHEN ((rt.typtype) :: text = ('d' :: character(1)) :: text) THEN 'DOMAIN' :: text
      WHEN ((rt.typtype) :: text = ('e' :: character(1)) :: text) THEN 'ENUM' :: text
      WHEN ((rt.typtype) :: text = ('r' :: character(1)) :: text) THEN 'RANGE' :: text
      WHEN ((rt.typtype) :: text = ('p' :: character(1)) :: text) THEN 'PSUEDO' :: text
      ELSE NULL :: text
    END AS return_type_type,
    p.proretset AS returns_set,
    ( SELECT
        COALESCE(json_agg(q.type_name), '[]')
      FROM
        (
          SELECT
            pt.typname AS type_name,
            pat.ordinality
          FROM
            unnest(
              COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
            ) WITH ORDINALITY pat(oid, ordinality)
            LEFT JOIN pg_type pt ON ((pt.oid = pat.oid))
          ORDER BY pat.ordinality ASC
        ) q
     ) AS input_arg_types,
    to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names,
    p.pronargdefaults AS default_args
  FROM
    pg_proc p
    JOIN pg_namespace pn ON (pn.oid = p.pronamespace)
    JOIN pg_type rt ON (rt.oid = p.prorettype)
    JOIN pg_namespace rtn ON (rtn.oid = rt.typnamespace)
  WHERE
    pn.nspname :: text NOT LIKE 'pg_%'
    AND pn.nspname :: text NOT IN ('information_schema', 'hdb_catalog', 'hdb_views')
    AND (NOT EXISTS (
            SELECT
              1
            FROM
              pg_aggregate
            WHERE
              ((pg_aggregate.aggfnoid) :: oid = p.oid)
          )
      )
  );

  CREATE VIEW hdb_catalog.hdb_function_info_agg AS (
    SELECT
      function_name,
      function_schema,
      row_to_json (
        (
          SELECT
            e
            FROM
                (
                  SELECT
                    has_variadic,
                    function_type,
                    return_type_schema,
                    return_type_name,
                    return_type_type,
                    returns_set,
                    input_arg_types,
                    input_arg_names,
                    default_args,
                    exists(
                      SELECT
                        1
                        FROM
                            information_schema.tables
                       WHERE
                  table_schema = return_type_schema
              AND table_name = return_type_name
                    ) AS returns_table
                ) AS e
        )
      ) AS "function_info"
      FROM
          hdb_catalog.hdb_function_agg
  );

  UPDATE hdb_catalog.hdb_version
     SET version = '22'
   WHERE version = '23';


From 22 to 21
"""""""""""""

.. code-block:: plpgsql

  DROP VIEW IF EXISTS hdb_catalog.hdb_function_info_agg;
  DROP VIEW IF EXISTS hdb_catalog.hdb_function_agg;

  CREATE VIEW hdb_catalog.hdb_function_agg AS
  (
  SELECT
    p.proname::text AS function_name,
    pn.nspname::text AS function_schema,

    CASE
      WHEN (p.provariadic = (0) :: oid) THEN false
      ELSE true
    END AS has_variadic,

    CASE
      WHEN (
        (p.provolatile) :: text = ('i' :: character(1)) :: text
      ) THEN 'IMMUTABLE' :: text
      WHEN (
        (p.provolatile) :: text = ('s' :: character(1)) :: text
      ) THEN 'STABLE' :: text
      WHEN (
        (p.provolatile) :: text = ('v' :: character(1)) :: text
      ) THEN 'VOLATILE' :: text
      ELSE NULL :: text
    END AS function_type,

    pg_get_functiondef(p.oid) AS function_definition,

    rtn.nspname::text AS return_type_schema,
    rt.typname::text AS return_type_name,

    CASE
      WHEN ((rt.typtype) :: text = ('b' :: character(1)) :: text) THEN 'BASE' :: text
      WHEN ((rt.typtype) :: text = ('c' :: character(1)) :: text) THEN 'COMPOSITE' :: text
      WHEN ((rt.typtype) :: text = ('d' :: character(1)) :: text) THEN 'DOMAIN' :: text
      WHEN ((rt.typtype) :: text = ('e' :: character(1)) :: text) THEN 'ENUM' :: text
      WHEN ((rt.typtype) :: text = ('r' :: character(1)) :: text) THEN 'RANGE' :: text
      WHEN ((rt.typtype) :: text = ('p' :: character(1)) :: text) THEN 'PSUEDO' :: text
      ELSE NULL :: text
    END AS return_type_type,
    p.proretset AS returns_set,
    ( SELECT
        COALESCE(json_agg(q.type_name), '[]')
      FROM
        (
          SELECT
            pt.typname AS type_name,
            pat.ordinality
          FROM
            unnest(
              COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
            ) WITH ORDINALITY pat(oid, ordinality)
            LEFT JOIN pg_type pt ON ((pt.oid = pat.oid))
          ORDER BY pat.ordinality ASC
        ) q
    ) AS input_arg_types,
    to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names
  FROM
    pg_proc p
    JOIN pg_namespace pn ON (pn.oid = p.pronamespace)
    JOIN pg_type rt ON (rt.oid = p.prorettype)
    JOIN pg_namespace rtn ON (rtn.oid = rt.typnamespace)
  WHERE
    pn.nspname :: text NOT LIKE 'pg_%'
    AND pn.nspname :: text NOT IN ('information_schema', 'hdb_catalog', 'hdb_views')
    AND (NOT EXISTS (
            SELECT
              1
            FROM
              pg_aggregate
            WHERE
              ((pg_aggregate.aggfnoid) :: oid = p.oid)
          )
      )
  );

  CREATE VIEW hdb_catalog.hdb_function_info_agg AS (
    SELECT
      function_name,
      function_schema,
      row_to_json (
        (
          SELECT
            e
            FROM
                (
                  SELECT
                    has_variadic,
                    function_type,
                    return_type_schema,
                    return_type_name,
                    return_type_type,
                    returns_set,
                    input_arg_types,
                    input_arg_names,
                    exists(
                      SELECT
                        1
                        FROM
                            information_schema.tables
                      WHERE
                  table_schema = return_type_schema
              AND table_name = return_type_name
                    ) AS returns_table
                ) AS e
        )
      ) AS "function_info"
      FROM
          hdb_catalog.hdb_function_agg
  );

  UPDATE hdb_catalog.hdb_version
    SET version = '21'
  WHERE version = '22';

From 21 to 20
"""""""""""""

.. code-block:: plpgsql

  DROP INDEX "event_log_locked_idx";

  UPDATE hdb_catalog.hdb_version
    SET version = '20'
  WHERE version = '21';

From 20 to 19
"""""""""""""

.. code-block:: plpgsql

  ALTER TABLE hdb_catalog.hdb_table DROP COLUMN is_enum;

  CREATE FUNCTION hdb_catalog.hdb_table_oid_check() RETURNS trigger AS
    $function$
  BEGIN
    IF (EXISTS (SELECT 1 FROM information_schema.tables st WHERE st.table_schema = NEW.table_schema AND st.table_name = NEW.table_name)) THEN
      return NEW;
    ELSE
      RAISE foreign_key_violation using message = 'table_schema, table_name not in information_schema.tables';
      return NULL;
    END IF;
  END;
  $function$
    LANGUAGE plpgsql;

  CREATE TRIGGER hdb_table_oid_check BEFORE INSERT OR UPDATE ON hdb_catalog.hdb_table
    FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.hdb_table_oid_check();


  DROP VIEW hdb_catalog.hdb_table_info_agg;
  DROP VIEW hdb_catalog.hdb_column;
  DROP VIEW hdb_catalog.hdb_foreign_key_constraint;

  CREATE VIEW hdb_catalog.hdb_foreign_key_constraint AS
  SELECT
      q.table_schema :: text,
      q.table_name :: text,
      q.constraint_name :: text,
      min(q.constraint_oid) :: integer as constraint_oid,
      min(q.ref_table_table_schema) :: text as ref_table_table_schema,
      min(q.ref_table) :: text as ref_table,
      json_object_agg(ac.attname, afc.attname) as column_mapping,
      min(q.confupdtype) :: text as on_update,
      min(q.confdeltype) :: text as on_delete
  FROM
      (SELECT
          ctn.nspname AS table_schema,
          ct.relname AS table_name,
          r.conrelid AS table_id,
          r.conname as constraint_name,
          r.oid as constraint_oid,
          cftn.nspname AS ref_table_table_schema,
          cft.relname as ref_table,
          r.confrelid as ref_table_id,
          r.confupdtype,
          r.confdeltype,
          UNNEST (r.conkey) AS column_id,
          UNNEST (r.confkey) AS ref_column_id
      FROM
          pg_catalog.pg_constraint r
          JOIN pg_catalog.pg_class ct
            ON r.conrelid = ct.oid
          JOIN pg_catalog.pg_namespace ctn
            ON ct.relnamespace = ctn.oid
          JOIN pg_catalog.pg_class cft
            ON r.confrelid = cft.oid
          JOIN pg_catalog.pg_namespace cftn
            ON cft.relnamespace = cftn.oid
      WHERE
          r.contype = 'f'
      ) q
      JOIN pg_catalog.pg_attribute ac
        ON q.column_id = ac.attnum
          AND q.table_id = ac.attrelid
      JOIN pg_catalog.pg_attribute afc
        ON q.ref_column_id = afc.attnum
          AND q.ref_table_id = afc.attrelid
  GROUP BY q.table_schema, q.table_name, q.constraint_name;

  CREATE VIEW hdb_catalog.hdb_table_info_agg AS (
  select
    tables.table_name as table_name,
    tables.table_schema as table_schema,
    coalesce(columns.columns, '[]') as columns,
    coalesce(pk.columns, '[]') as primary_key_columns,
    coalesce(constraints.constraints, '[]') as constraints,
    coalesce(views.view_info, 'null') as view_info
  from
    information_schema.tables as tables
    left outer join (
      select
        c.table_name,
        c.table_schema,
        json_agg(
          json_build_object(
            'name',
            column_name,
            'type',
            udt_name,
            'is_nullable',
            is_nullable :: boolean
          )
        ) as columns
      from
        information_schema.columns c
      group by
        c.table_schema,
        c.table_name
    ) columns on (
      tables.table_schema = columns.table_schema
      AND tables.table_name = columns.table_name
    )
    left outer join (
      select * from hdb_catalog.hdb_primary_key
    ) pk on (
      tables.table_schema = pk.table_schema
      AND tables.table_name = pk.table_name
    )
    left outer join (
      select
        c.table_schema,
        c.table_name,
        json_agg(constraint_name) as constraints
      from
        information_schema.table_constraints c
      where
        c.constraint_type = 'UNIQUE'
        or c.constraint_type = 'PRIMARY KEY'
      group by
        c.table_schema,
        c.table_name
    ) constraints on (
      tables.table_schema = constraints.table_schema
      AND tables.table_name = constraints.table_name
    )
    left outer join (
      select
        table_schema,
        table_name,
        json_build_object(
          'is_updatable',
          (is_updatable::boolean OR is_trigger_updatable::boolean),
          'is_deletable',
          (is_updatable::boolean OR is_trigger_deletable::boolean),
          'is_insertable',
          (is_insertable_into::boolean OR is_trigger_insertable_into::boolean)
        ) as view_info
      from
        information_schema.views v
    ) views on (
      tables.table_schema = views.table_schema
      AND tables.table_name = views.table_name
    )
  );

  UPDATE hdb_catalog.hdb_version
    SET version = '19'
  WHERE version = '20';

From 19 to 18
"""""""""""""

.. code-block:: plpgsql

  DROP TRIGGER hdb_schema_update_event_notifier ON hdb_catalog.hdb_schema_update_event;

  CREATE TRIGGER hdb_schema_update_event_notifier AFTER INSERT ON
    hdb_catalog.hdb_schema_update_event FOR EACH ROW EXECUTE PROCEDURE
                        hdb_catalog.hdb_schema_update_event_notifier();

  DROP INDEX "hdb_schema_update_event_one_row";

  ALTER TABLE hdb_catalog.hdb_schema_update_event ADD COLUMN id SERIAL PRIMARY KEY;

  UPDATE hdb_catalog.hdb_version
    SET version = '18'
  WHERE version = '19';

From 18 to 17
"""""""""""""

.. code-block:: plpgsql

  CREATE TABLE hdb_catalog.hdb_query_template
  (
    template_name TEXT PRIMARY KEY,
    template_defn JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false
  );

  INSERT INTO hdb_catalog.hdb_table (table_schema, table_name)
              VALUES ('hdb_catalog', 'hdb_query_template');

  UPDATE hdb_catalog.hdb_version
    SET version = '17'
  WHERE version = '18';

From 17 to 16
"""""""""""""

.. code-block:: plpgsql

  UPDATE hdb_catalog.hdb_table
    SET is_system_defined = 'false'
  WHERE table_schema = 'hdb_catalog'
        AND  table_name = 'hdb_allowlist';

  UPDATE hdb_catalog.hdb_version
    SET version = '16'
  WHERE version = '17';

From 16 to 15
"""""""""""""

.. code-block:: plpgsql

  DELETE FROM hdb_catalog.hdb_table WHERE (table_schema, table_name)
  IN (('hdb_catalog', 'hdb_query_collection'), ('hdb_catalog', 'hdb_allowlist'));

  DROP TABLE IF EXISTS hdb_catalog.hdb_allowlist;
  DROP TABLE IF EXISTS hdb_catalog.hdb_query_collection;

  UPDATE hdb_catalog.hdb_version
    SET version = '15'
  WHERE version = '16';

From 15 to 14
"""""""""""""

.. code-block:: plpgsql

  DROP FUNCTION IF EXISTS hdb_catalog.insert_event_log(text, text, text, text, json);

  UPDATE hdb_catalog.hdb_version
    SET version = '14'
  WHERE version = '15';


From 14 to 13
"""""""""""""

.. code-block:: plpgsql

  DROP VIEW IF EXISTS hdb_catalog.hdb_table_info_agg;
  DROP VIEW IF EXISTS hdb_catalog.hdb_function_info_agg;

  UPDATE hdb_catalog.hdb_version
    SET version = '13'
  WHERE version = '14';


From 13 to 12
"""""""""""""

.. code-block:: plpgsql

  CREATE OR REPLACE VIEW hdb_catalog.hdb_function_agg AS
  (
  SELECT
    p.proname::text AS function_name,
    pn.nspname::text AS function_schema,

    CASE
      WHEN (p.provariadic = (0) :: oid) THEN false
      ELSE true
    END AS has_variadic,

    CASE
      WHEN (
        (p.provolatile) :: text = ('i' :: character(1)) :: text
      ) THEN 'IMMUTABLE' :: text
      WHEN (
        (p.provolatile) :: text = ('s' :: character(1)) :: text
      ) THEN 'STABLE' :: text
      WHEN (
        (p.provolatile) :: text = ('v' :: character(1)) :: text
      ) THEN 'VOLATILE' :: text
      ELSE NULL :: text
    END AS function_type,

    pg_get_functiondef(p.oid) AS function_definition,

    rtn.nspname::text AS return_type_schema,
    rt.typname::text AS return_type_name,

    CASE
      WHEN ((rt.typtype) :: text = ('b' :: character(1)) :: text) THEN 'BASE' :: text
      WHEN ((rt.typtype) :: text = ('c' :: character(1)) :: text) THEN 'COMPOSITE' :: text
      WHEN ((rt.typtype) :: text = ('d' :: character(1)) :: text) THEN 'DOMAIN' :: text
      WHEN ((rt.typtype) :: text = ('e' :: character(1)) :: text) THEN 'ENUM' :: text
      WHEN ((rt.typtype) :: text = ('r' :: character(1)) :: text) THEN 'RANGE' :: text
      WHEN ((rt.typtype) :: text = ('p' :: character(1)) :: text) THEN 'PSUEDO' :: text
      ELSE NULL :: text
    END AS return_type_type,
    p.proretset AS returns_set,
    ( SELECT
        COALESCE(json_agg(pt.typname), '[]')
      FROM
        (
          unnest(
            COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
          ) WITH ORDINALITY pat(oid, ordinality)
          LEFT JOIN pg_type pt ON ((pt.oid = pat.oid))
        )
    ) AS input_arg_types,
    to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names
  FROM
    pg_proc p
    JOIN pg_namespace pn ON (pn.oid = p.pronamespace)
    JOIN pg_type rt ON (rt.oid = p.prorettype)
    JOIN pg_namespace rtn ON (rtn.oid = rt.typnamespace)
  WHERE
    pn.nspname :: text NOT LIKE 'pg_%'
    AND pn.nspname :: text NOT IN ('information_schema', 'hdb_catalog', 'hdb_views')
    AND (NOT EXISTS (
            SELECT
              1
            FROM
              pg_aggregate
            WHERE
              ((pg_aggregate.aggfnoid) :: oid = p.oid)
          )
      )
  );

  UPDATE hdb_catalog.hdb_version
    SET version = '12'
  WHERE version = '13';

From 12 to 11
"""""""""""""

.. code-block:: plpgsql

  ALTER TABLE hdb_catalog.event_triggers
    DROP CONSTRAINT event_triggers_pkey;

  ALTER TABLE hdb_catalog.event_triggers
    ADD COLUMN id TEXT DEFAULT gen_random_uuid();

  ALTER TABLE hdb_catalog.event_triggers
    ADD PRIMARY KEY (id);

  ALTER TABLE hdb_catalog.event_triggers
    ADD UNIQUE (name);

  ALTER TABLE hdb_catalog.event_log
    ADD COLUMN trigger_id TEXT;

  UPDATE hdb_catalog.event_log as e
    SET trigger_id = (SELECT id FROM hdb_catalog.event_triggers et WHERE et.name = e.trigger_name);

  ALTER TABLE hdb_catalog.event_log
    ALTER COLUMN trigger_id SET NOT NULL;

  DROP INDEX hdb_catalog.event_log_trigger_name_idx;

  UPDATE hdb_catalog.hdb_relationship
    SET rel_def = '{"manual_configuration":{"remote_table":{"schema":"hdb_catalog","name":"event_log"},"column_mapping":{"id":"trigger_id"}}}'
  WHERE table_schema = 'hdb_catalog'
        AND table_name = 'event_triggers'
        AND rel_name = 'events';

  UPDATE hdb_catalog.hdb_relationship
    SET rel_def = '{"manual_configuration":{"remote_table":{"schema":"hdb_catalog","name":"event_triggers"},"column_mapping":{"trigger_id":"id"}}}'
  WHERE table_schema = 'hdb_catalog'
        AND table_name = 'event_log'
        AND rel_name = 'trigger';

  UPDATE hdb_catalog.hdb_version
    SET version = '11'
  WHERE version = '12';

From 11 to 10
"""""""""""""

.. code-block:: plpgsql

  DROP TRIGGER IF EXISTS hdb_schema_update_event_notifier
    ON hdb_catalog.hdb_schema_update_event CASCADE;

  DROP TABLE IF EXISTS hdb_catalog.hdb_schema_update_event;

  DROP FUNCTION IF EXISTS hdb_catalog.hdb_schema_update_event_notifier;

  UPDATE hdb_catalog.hdb_version
    SET version = '10'
  WHERE version = '11';

From 10 to 9
""""""""""""

.. code-block:: plpgsql

  ALTER TABLE hdb_catalog.hdb_relationship
    DROP CONSTRAINT hdb_relationship_table_schema_fkey,
    ADD CONSTRAINT hdb_relationship_table_schema_fkey FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name);

  ALTER TABLE hdb_catalog.hdb_permission
    DROP CONSTRAINT hdb_permission_table_schema_fkey,
    ADD CONSTRAINT hdb_permission_table_schema_fkey FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name);

  ALTER TABLE hdb_catalog.event_triggers
    ADD CONSTRAINT event_triggers_table_schema_fkey FOREIGN KEY (schema_name, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name);

  UPDATE hdb_catalog.hdb_version
    SET version = '9'
  WHERE version = '10';

From 9 to 8
"""""""""""

.. code-block:: plpgsql

  ALTER TABLE hdb_catalog.hdb_version
    DROP CONSTRAINT hdb_version_pkey,
    DROP COLUMN console_state,
    DROP COLUMN cli_state,
    DROP COLUMN hasura_uuid;

  DELETE FROM hdb_catalog.hdb_table
  WHERE table_name = 'hdb_version'
    AND table_schema = 'hdb_catalog';

  UPDATE hdb_catalog.hdb_version
    SET version = '8'
  WHERE version = '9';

From 8 to 7
"""""""""""

.. code-block:: plpgsql

  DROP TABLE IF EXISTS hdb_catalog.hdb_function;
  DROP VIEW IF EXISTS hdb_catalog.hdb_function_agg;

  DELETE FROM hdb_catalog.hdb_relationship
  WHERE table_schema = 'hdb_catalog'
    AND table_name = 'hdb_function_agg'
    AND rel_name = 'return_table_info';

  DELETE FROM hdb_catalog.hdb_table
  WHERE (table_schema, table_name) IN
        (('hdb_catalog', 'hdb_function_agg'), ('hdb_catalog', 'hdb_function'));

  UPDATE hdb_catalog.hdb_version
    SET version = '7'
  WHERE version = '8';

From 7 to 6
"""""""""""

.. code-block:: plpgsql

  CREATE OR REPLACE VIEW hdb_catalog.hdb_primary_key AS
    SELECT
      tc.table_schema,
      tc.table_name,
      tc.constraint_name,
      json_agg(ccu.column_name) as columns
      FROM
          information_schema.table_constraints tc
          JOIN information_schema.constraint_column_usage ccu
              ON tc.constraint_name = ccu.constraint_name
    WHERE
      constraint_type = 'PRIMARY KEY'
    GROUP BY
      tc.table_schema, tc.table_name, tc.constraint_name;

  UPDATE hdb_catalog.hdb_version
    SET version = '6'
  WHERE version = '7';


From 6 to 5
"""""""""""

.. code-block:: plpgsql

  CREATE OR REPLACE FUNCTION hdb_catalog.first_agg ( anyelement, anyelement )
    RETURNS anyelement LANGUAGE SQL IMMUTABLE STRICT AS $$
    SELECT $1;
    $$;

    CREATE AGGREGATE hdb_catalog.FIRST (
      sfunc    = hdb_catalog.first_agg,
      basetype = anyelement,
      stype    = anyelement
    );

  CREATE OR REPLACE FUNCTION hdb_catalog.last_agg ( anyelement, anyelement )
    RETURNS anyelement LANGUAGE SQL IMMUTABLE STRICT AS $$
    SELECT $2;
    $$;

    CREATE AGGREGATE hdb_catalog.LAST (
      sfunc    = hdb_catalog.last_agg,
      basetype = anyelement,
      stype    = anyelement
    );

  CREATE OR REPLACE VIEW hdb_catalog.hdb_foreign_key_constraint AS
  SELECT
      q.table_schema :: text,
      q.table_name :: text,
      q.constraint_name :: text,
      hdb_catalog.first(q.constraint_oid) :: integer as constraint_oid,
      hdb_catalog.first(q.ref_table_table_schema) :: text as ref_table_table_schema,
      hdb_catalog.first(q.ref_table) :: text as ref_table,
      json_object_agg(ac.attname, afc.attname) as column_mapping,
      hdb_catalog.first(q.confupdtype) :: text as on_update,
      hdb_catalog.first(q.confdeltype) :: text as on_delete
  FROM
      (SELECT
          ctn.nspname AS table_schema,
          ct.relname AS table_name,
          r.conrelid AS table_id,
          r.conname as constraint_name,
          r.oid as constraint_oid,
          cftn.nspname AS ref_table_table_schema,
          cft.relname as ref_table,
          r.confrelid as ref_table_id,
          r.confupdtype,
          r.confdeltype,
          UNNEST (r.conkey) AS column_id,
          UNNEST (r.confkey) AS ref_column_id
      FROM
          pg_catalog.pg_constraint r
          JOIN pg_catalog.pg_class ct
            ON r.conrelid = ct.oid
          JOIN pg_catalog.pg_namespace ctn
            ON ct.relnamespace = ctn.oid
          JOIN pg_catalog.pg_class cft
            ON r.confrelid = cft.oid
          JOIN pg_catalog.pg_namespace cftn
            ON cft.relnamespace = cftn.oid
      WHERE
          r.contype = 'f'
      ) q
      JOIN pg_catalog.pg_attribute ac
        ON q.column_id = ac.attnum
          AND q.table_id = ac.attrelid
      JOIN pg_catalog.pg_attribute afc
        ON q.ref_column_id = afc.attnum
          AND q.ref_table_id = afc.attrelid
  GROUP BY q.table_schema, q.table_name, q.constraint_name;

  UPDATE hdb_catalog.hdb_version
    SET version = '5'
  WHERE version = '6';

From 5 to 4
"""""""""""

.. code-block:: plpgsql

  DROP TABLE IF EXISTS hdb_catalog.remote_schemas;

  DELETE FROM hdb_catalog.hdb_table
  WHERE table_schema = 'hdb_catalog'
    AND table_name = 'remote_schemas';

  UPDATE hdb_catalog.hdb_version
    SET version = '4'
  WHERE version = '5';

From 4 to 3
"""""""""""

.. code-block:: plpgsql

  ALTER TABLE hdb_catalog.event_triggers
    ADD COLUMN definition JSON,
    ADD COLUMN query TEXT,
    ADD COLUMN webhook TEXT,
    ADD COLUMN num_retries INTEGER DEFAULT 0,
    ADD COLUMN retry_interval INTEGER DEFAULT 10,
    ADD COLUMN headers JSON;

  UPDATE hdb_catalog.event_triggers AS et
    SET ( definition
        , webhook
        , num_retries
        , retry_interval
        , headers
        ) = ( SELECT (e.configuration::json -> 'definition')::json as definition,
                      (e.configuration::json ->> 'webhook')::text as webhook,
                      (e.configuration::json #>> '{retry_conf,num_retries}'::text[])::int as num_retries,
                      (e.configuration::json #>> '{retry_conf,interval_sec}'::text[])::int as retry_interval,
                      (e.configuration::json -> 'headers')::json as headers
                  FROM hdb_catalog.event_triggers AS e
                WHERE e.name = et.name
            );

  ALTER TABLE hdb_catalog.event_triggers
    DROP COLUMN configuration,
    ALTER COLUMN webhook SET NOT NULL;


  UPDATE hdb_catalog.hdb_version
    SET version = '3'
  WHERE version = '4';


From 3 to 2
"""""""""""

.. code-block:: plpgsql

  DROP INDEX IF EXISTS event_invocation_logs_event_id_idx;
  DROP INDEX IF EXISTS event_log_trigger_name_idx;

  ALTER TABLE hdb_catalog.event_log DROP COLUMN next_retry_at;
  ALTER TABLE hdb_catalog.event_triggers DROP COLUMN headers;

  UPDATE hdb_catalog.hdb_version
    SET version = '2'
  WHERE version = '3';

From 2 to 1
"""""""""""

.. code-block:: plpgsql

  DROP TABLE IF EXISTS hdb_catalog.event_invocation_logs;
  DROP TABLE IF EXISTS hdb_catalog.event_log;
  DROP TABLE IF EXISTS hdb_catalog.event_triggers;

  DELETE FROM hdb_catalog.hdb_relationship
  WHERE (table_schema, table_name) IN
        ( ('hdb_catalog', 'event_log')
        , ('hdb_catalog', 'event_triggers')
        , ('hdb_catalog', 'event_invocation_logs')
        );

  DELETE FROM hdb_catalog.hdb_table
  WHERE (table_schema, table_name) IN
        ( ('hdb_catalog', 'event_log')
        , ('hdb_catalog', 'event_triggers')
        , ('hdb_catalog', 'event_invocation_logs')
        );

  UPDATE hdb_catalog.hdb_version
    SET version = '1'
  WHERE version = '2';
