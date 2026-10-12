# Hasura project instructions

This is a Hasura GraphQL Engine (v2) CLI project, not a Hasura DDN project.
Develop database migrations and Hasura metadata by editing version-controlled files;
**do not start `hasura console` or rely on the web Console to generate changes**.
This guide is a starting point: preserve the project's existing conventions and
update it when the project gains its own policies.

## Inspect the project first

- Read `config.yaml` for the config version, endpoint and custom directory paths.
  Run commands from the directory containing that file (or use `--project <path>`).
  CLI config version and `metadata/version.yaml` are different version numbers;
  do not change either just to add a feature.
- Inspect existing metadata, migrations, database schema and application code before
  choosing source names, schemas, table names, relationships or roles. Do not invent
  columns, session variables, credentials or backend capabilities.
- Database schema changes belong in SQL migrations. Tracking tables, relationships,
  permissions, actions and other Hasura configuration belong in metadata. SQL
  migrations do not automatically track tables or create permissions.
- Generate and review files freely, but apply changes only to an explicitly approved
  development/test endpoint. Production applies, destructive SQL, seed execution and
  rollbacks require explicit authorization. If no server is available, generate
  files and report which checks could not be run; never claim they passed.
- Never put secrets in this file, SQL, metadata or Git. Use `HASURA_GRAPHQL_ADMIN_SECRET`
  for CLI authentication and supported `from_env`
  configuration for database URLs and webhook headers. These metadata environment
  variables must exist on the **Hasura server**, not just in the local shell.
  `hasura init --admin-secret` stores the secret in `config.yaml`; remove it before
  committing and use an environment variable instead.

## Project structure

This project was initialized with **CLI config v3** (multi-source layout):

```text
config.yaml
AGENTS.md
metadata/
  version.yaml
  databases/
    databases.yaml                     # source definitions and include references
    <source-name>/
      tables/
        tables.yaml                    # list of included table metadata files
        <schema>_<table>.yaml
      functions/
        functions.yaml                 # list of included function metadata files
        <schema>_<function>.yaml
  actions.yaml                         # action configuration and custom types
  actions.graphql                      # action/custom type GraphQL definitions
  remote_schemas.yaml
  query_collections.yaml
  allow_list.yaml
  cron_triggers.yaml
migrations/
  <source-name>/
    <timestamp>_<description>/
      up.sql
      down.sql
seeds/
  <source-name>/
    <name>.sql
```

Fresh initialization has an empty source list; source-specific directories are
created when sources/tables/functions are added or exported. Additional root metadata
files (REST endpoints, inherited roles, API limits, etc.) depend on server features.
Use the exact source `name` in `databases.yaml`, not the physical database name.

## Metadata authoring conventions

- Preserve all unrelated metadata, ordering and formatting; make a minimal diff.
  Use two-space YAML indentation and follow the existing naming style.
- `!include` paths are resolved relative to the containing YAML file. Keep references
  valid and inside the metadata directory. Hasura's include syntax is not ordinary
  YAML: a generic YAML parser alone does not assemble or validate the project.
- For split table/function metadata, add both the object file **and** its entry in
  `tables.yaml`/`functions.yaml`. A YAML file not referenced by its index is not
  automatically included.
- A PostgreSQL source may reference its tables like this in `databases.yaml`
  (adapt the source name and environment variable; never replace existing sources):

  ```yaml
  - name: default
    kind: postgres
    configuration:
      connection_info:
        database_url:
          from_env: HASURA_DATABASE_URL
    tables: "!include default/tables/tables.yaml"
  ```

  `metadata/databases/default/tables/tables.yaml` then contains:

  ```yaml
  - "!include public_article.yaml"
  ```

  And `metadata/databases/default/tables/public_article.yaml` contains the table
  object shown below. Use an empty list (`[]`) for an empty index.

- Example PostgreSQL table object (assumes `public.article`, its columns, the `user`
  role and the authenticated user's session variable are part of the intended design):

  ```yaml
  table:
    schema: public
    name: article
  select_permissions:
    - role: user
      permission:
        columns:
          - id
          - title
          - author_id
        filter:
          author_id:
            _eq: X-Hasura-User-Id
  ```

- Design permissions with least privilege: explicit column lists, row filters,
  insert checks, update filters/checks and column presets where appropriate.
  An empty filter/check (`{}`) allows every row; `columns: "*"` allows every column.
  Do not broaden access or add aggregation/root-field access without a requirement.
  Verify behavior as the intended role, not only with an admin-secret request.
- Relationships must match real foreign keys or a deliberate manual configuration.
  In PostgreSQL, an object relationship using a local foreign key and an array
  relationship using the referencing table have different shapes. For example, if
  `public.article.author_id` references `public.author.id`:

  ```yaml
  # in public_article.yaml
  object_relationships:
    - name: author
      using:
        foreign_key_constraint_on: author_id
  ```

  ```yaml
  # in public_author.yaml
  array_relationships:
    - name: articles
      using:
        foreign_key_constraint_on:
          table:
            schema: public
            name: article
          column: author_id
  ```

  For other shapes (manual configuration, composite keys), copy a correct existing
  example or consult the metadata reference rather than guessing.
- Preserve source `kind` and backend-specific configuration. The PostgreSQL examples
  here must not be copied blindly for SQL Server, BigQuery or data connectors.
  SQL migrations are not supported uniformly across all backends.
- Keep actions' `actions.graphql` definitions and `actions.yaml` configuration/custom
  types consistent. Preserve existing remote schemas, event/cron triggers,
  allow lists, REST endpoints and inherited roles.
- Removing a table/column/function also requires removing or updating dependent
  permissions, relationships, computed fields and other metadata. A down SQL
  migration does not roll back metadata automatically.

## Migration conventions

- Never edit, renumber or delete an already-applied/shared migration. Add a new
  migration with a unique Unix-millisecond timestamp and a descriptive snake_case
  name. Follow the ordering and SQL dialect used in the project.
- Put the forward schema change in `up.sql` and a deliberate reverse change in
  `down.sql`. If reversal would destroy data or is not possible, explain the
  limitation and obtain agreement; do not present it as a safe rollback.
- Prefer staged, backward-compatible changes for populated tables. Consider locks,
  data backfills, constraints and deployment ordering. Do not use `DROP ... CASCADE`,
  truncate data or change Hasura's `hdb_catalog`/`hdb_views` internals as shortcuts.
- Metadata-only changes do not need empty SQL migrations. Keep seeds separate from
  schema migrations; never run them automatically.

To ask the CLI to scaffold empty migration files against a reachable development
server (this creates files, not the schema change):

```bash
hasura migrate create add_article --database-name <source-name>
```

Fill in both generated SQL files, or pass the SQL directly with
`--up-sql "<sql>" --down-sql "<sql>"`. Without a server, create the same directory
and files directly using a unique timestamp; do not invent a CLI `--offline` flag.
Other CLI commands, including migration scaffolding and metadata dry-run, may
contact the configured server for initialization/version checks.

### PostgreSQL: large or hot tables

Each migration file runs inside its own transaction, so locks taken by a statement
are held until the whole file finishes.

- **Index builds**: a plain `CREATE INDEX` takes a `SHARE` lock (blocks writes) for
  the whole scan, bounded only by `statement_timeout` if one is set (the PostgreSQL
  default is no limit). That is acceptable for small/medium tables; a failed or
  timed-out build rolls back cleanly. For a very large or hot table, use
  `CREATE INDEX CONCURRENTLY IF NOT EXISTS` in a migration that runs without a
  transaction (see below). A failed concurrent build can leave an `INVALID` index
  that must be inspected and dropped before retrying.
- **Adding a CHECK or FOREIGN KEY constraint**: a plain `ADD CONSTRAINT` scans the
  whole table while holding a strong lock (`ACCESS EXCLUSIVE` for CHECK,
  `SHARE ROW EXCLUSIVE` for FK), blocking writes. Split it across **two
  migrations**: `ADD CONSTRAINT … NOT VALID` (brief lock, no scan; new rows are
  still checked) in the first, then `VALIDATE CONSTRAINT` (scans under
  `SHARE UPDATE EXCLUSIVE`, which does not block reads or writes) in the next.
  Putting both in one file gains nothing, because the strong lock is held until
  the transaction ends.
- **Adding NOT NULL**: before PostgreSQL 18 there is no `NOT VALID` form of
  `SET NOT NULL`. Use a check constraint instead:

  ```sql
  -- migration 1 (up.sql)
  ALTER TABLE public.article
    ADD CONSTRAINT article_title_not_null CHECK (title IS NOT NULL) NOT VALID;

  -- migration 2 (up.sql)
  ALTER TABLE public.article VALIDATE CONSTRAINT article_title_not_null;
  -- PostgreSQL 12+ uses the validated check to skip the full-table scan:
  ALTER TABLE public.article ALTER COLUMN title SET NOT NULL;
  ALTER TABLE public.article DROP CONSTRAINT article_title_not_null;
  ```

**Running a migration without a transaction** (required for `CONCURRENTLY`):
with HGE/CLI v2.51.0 or later (check with `hasura version`), make
`-- hasura:no-transaction` the exact **first line** of that `up.sql`/`down.sql` and
apply with the per-migration transaction flag:

```bash
hasura migrate apply --database-name <source-name> --per-migration-transaction
```

(or set `HASURA_GRAPHQL_PER_MIGRATION_TRANSACTION=true`). Without the flag the
marker is ignored and `CONCURRENTLY` fails inside the transaction. The flag cannot
be combined with `--no-transaction` and is not supported on MSSQL or BigQuery
sources. Keep each no-transaction migration to a single idempotent statement,
because a failure part-way leaves earlier statements applied.

## Console-free development and validation

1. Confirm the target endpoint, source and credentials. Review `git status` first.
2. For an existing server whose metadata is not yet in the repository, export a
   baseline **before** editing (`hasura metadata export`) and review it. Export
   overwrites local metadata: do not export over uncommitted metadata work.
3. Generate the SQL migrations and edit the metadata files, including include indexes.
   Inspect the Git diff and check SQL syntax and metadata references.
4. On an authorized development instance where the source is already registered,
   inspect migration status, apply forward SQL migrations, then apply metadata
   that tracks the new schema:

   ```bash
   hasura migrate status --database-name <source-name>
   hasura migrate apply --database-name <source-name>
   hasura metadata apply --dry-run
   hasura metadata diff
   hasura metadata apply --disallow-inconsistent-metadata
   hasura metadata inconsistency status
   hasura migrate status --database-name <source-name>
   ```

   `--dry-run` assembles/prints metadata without applying it; it is **not** a
   server-side schema or permission validation. `metadata diff` is for review,
   not a guaranteed nonzero exit-code check when differences exist.
   `metadata apply` replaces the **entire** server metadata, not just edited files.
   Never apply a partial or empty local baseline to a populated server.

   For a brand-new source, register the source with empty tracking first: apply
   metadata in which that source's `tables` (and `functions`) index is `[]`, then
   apply its migrations, then restore the table entries and apply the complete
   local metadata.
   Do not apply metadata that tracks tables before those tables exist.

   For destructive changes, first remove/update dependent metadata on the server,
   then perform the SQL change. Rollback likewise needs coordinated metadata and
   SQL ordering; the forward sequence above is not a universal rollback recipe.

5. Check GraphQL operations and permissions using the application's real
   authentication/session setup and intended roles. Where practical, replay on a
   fresh development database to check reproducibility and test rollback safely.
6. Report files changed, migrations generated, commands actually run, test results
   and unverified steps. Commit related SQL and metadata changes together.

## References

Use documentation matching the deployed engine version and backend:

- Metadata format: https://hasura.io/docs/2.0/migrations-metadata-seeds/metadata-format/
- Metadata API: https://hasura.io/docs/2.0/api-reference/metadata-api/
- Migrations: https://hasura.io/docs/2.0/migrations-metadata-seeds/manage-migrations/
- Metadata best practices: https://hasura.io/docs/2.0/migrations-metadata-seeds/metadata-best-practices/
