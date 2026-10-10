# AGENTS.md (shared/types)

Nx project `shared-types`, import path `@hasura/shared/types`. Types-only,
zero-runtime-logic library: shared TypeScript types, a couple of error
classes, and constants used across the console frontend.

## Public API (`src/index.ts`)

Re-exports everything from 5 modules, no other structure/subfolders:

- `./constants` — string/object constants
- `./env` — `EnvVars` discriminated union + related types
- `./graphql` — GraphQL/Graphiql request & ordering types
- `./error` — `HttpError`, `UnauthorizedError` classes
- `./types` — generic TS utility types

## `env.ts` — `EnvVars`

`EnvVars` is a large base object type (all fields optional except
`serverVersion`, `schemaRegistryHost`) intersected (`&`) with a union of
console-variant types:

- `OSSServerEnv` — `consoleMode: 'server'`, `consoleType: 'oss'`
- `CloudServerEnv` — `consoleMode: 'server'`, `consoleType: 'cloud'` (has
  `tenantID`, `projectID`, `userRole: 'owner'|'user'`, `luxDataHost`,
  optional Neon/Slack OAuth fields, `allowedLuxFeatures?: LuxFeature[]`)
- `ProServerEnv` — `consoleMode: 'server'`, `consoleType: 'pro'`
- `ProLiteServerEnv` — `consoleMode: 'server'`, `consoleType: 'pro-lite'`
- `OSSCliEnv` — `consoleMode: 'cli'` (no `consoleType` field at all)
- `CloudCliEnv` (exported) — `consoleMode: 'cli'`, plus a literal `pro: true`
  field and `projectId`
- `ProCliEnv` = `ProLiteCliEnv` = type aliases of `CloudCliEnv` (not distinct
  shapes)

Discriminants: server variants key on `consoleType`
(`oss`/`cloud`/`pro`/`pro-lite`), all with `consoleMode: 'server'`. CLI
variants key on `consoleMode: 'cli'`; `OSSCliEnv` has no `consoleType`, while
`CloudCliEnv`/`ProCliEnv`/`ProLiteCliEnv` are identical (`pro: true` literal,
no field distinguishing cloud/pro/pro-lite in CLI mode) — this is a known,
acknowledged gap (commented in source), not an oversight to "fix" without
checking with the team.

Also in `env.ts`: `LuxFeature` (open string union of known feature flags),
`ConsoleType`, `ConsoleMode`.

## Other type modules

- `types.ts` — generic helpers: `Nullable`, `MakeNever`, `DiscriminatedTypes`,
  `Path`/`PathValue` (dot-path typing), `PartialBy`, `DistributiveOmit`,
  `PathInto`/`Choose`, `CreateBooleanMap`, `NameValue`. Heavily commented with
  usage examples.
- `graphql.ts` — `GraphiqlMode`, `WhereClause`, `GraphQLRequestInput`,
  `DataHeader`, `OrderBy`/`OrderByType`/`OrderByNulls`.
- `error.ts` — `HttpError` (status: `number | 'network-error'`, generic
  `data`), `UnauthorizedError`.
- `constants.ts` — `SERVER_CONSOLE_MODE`/`CLI_CONSOLE_MODE`, header name
  constants, `LOGIN_PATH`, `LS_KEYS` (large map of localStorage key names).

## Gotchas

- `HASURA_SSO_TOKEN` and `HASURA_COLLABORATOR_TOKEN` constants share the
  identical string value `'hasura-collaborator-token'` — likely intentional
  aliasing but easy to misread as a bug.
- When building a mock `EnvVars` for a specific console mode (e.g. in
  Storybook), override **all** discriminant fields for that variant together
  (`consoleMode` + `consoleType` + that variant's required fields) — partial
  overrides across variants produce confusing union-assignability errors.

## Testing

Vitest is fully configured (`vitest.config.mts`, `project.json` test target),
but **no test files exist** — expected for a types-only package with no
runtime logic.
