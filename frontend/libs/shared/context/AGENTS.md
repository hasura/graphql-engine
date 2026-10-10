# AGENTS.md (shared/context)

Nx project `shared-context`, import path `@hasura/shared/context`. Source is
flat under `src/` (no `lib` subfolder) — `src/index.ts` re-exports everything
from `app.ts`, `auth.ts`, and `endpoints.ts`. No other files exist.

## Purpose

Provides the console's core React contexts (app state, auth state) and a pure
function that derives all backend/service URLs (`Endpoints`) from environment
variables and a base URL. This is the shared source of truth for "where is
the server / what mode is the app in" across the console frontend.

## Exports

**`app.ts`** — `AppContext` + `useAppContext()`.

- `AppState`: `{ readOnlyMode, serverVersion, isProduction, latestServerVersion, featuresCompatibility, envVars: EnvVars, endpoints: Endpoints }`
- `defaultAppState` casts `envVars`/`endpoints` to `{} as EnvVars` / `{} as Endpoints` — type-unsafe; a component that forgets to wrap in `AppContext.Provider` gets objects that lie about having required fields.

**`auth.ts`** — `AuthContext` + `useAuthContext()`.

- `AuthService<S, AT = 'none' | 'admin-secret'>`: `{ isAuthenticated, authType, hasuraUserId?, getHeaders(): Promise<Record<string,string>>, authenticate(input: S): Promise<boolean>, logout(): void }`
- Default context value is fully "authenticated, no-op" (`isAuthenticated: true`, `authType: 'none'`) — silently no-ops if a component forgets to wrap in a real `AuthContext.Provider`, rather than failing loudly.

**`endpoints.ts`** — `getEndpoints(globals: EnvVars, baseUrl: string)` builds
~20 URLs (GraphQL, Relay, metadata, migrate, telemetry, notifications,
license, prometheus, schema registry, etc.) via string-templating.
`Endpoints = ReturnType<typeof getEndpoints>`.

## Gotchas

- `getEndpoints` reads `window.location.protocol` directly (for
  `luxDataGraphql`, `luxDataGraphqlWs`, `schemaRegistry`) — not SSR-safe,
  requires a browser environment.
- WebSocket URLs go through `replaceURLHttpToWs`/`getWebsocketProtocol` from
  `@hasura/shared/utils`, not hand-built.
- Several endpoints are hardcoded absolute external URLs (`updateCheck`,
  `telemetryServer`, `consoleNotificationsProd/Stg`, `registerEETrial`)
  regardless of `baseUrl`.
- Depends on `@hasura/shared/utils`, `@hasura/shared/types`, `@hasura/shared/types`.

## Testing

Vitest is configured (`vitest.config.mts`) but **no test files currently
exist** in this package.
