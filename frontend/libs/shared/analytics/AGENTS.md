# AGENTS.md (shared/analytics)

**Nx project name is `analytics`, not `shared-analytics`** — unlike every
sibling (`shared-context`, `shared-hooks`, `shared-ui`, etc.). Import path is
still `@hasura/shared/analytics`. No comment in `project.json`/history
explains the inconsistency — most likely legacy naming never renamed to
match the later `shared-` convention, not an intentional choice.

## Purpose

Shared abstraction for instrumenting UI analytics/telemetry across Hasura
console apps: HTML data-attributes for tracking (Heap), custom event
tracking (Heap + Sentry breadcrumbs), error tracing (Sentry), and generic
click/change telemetry listeners used by the CE/EE console.

## Public API (`src/index.ts`)

- **Core utilities:** `startTracing(envVars)` — idempotently starts Sentry
  tracing (Heap is server-injected, not started here); `addUserProperties(props)`
  — sets Heap user properties; `parseSentryDsn`; `REDACT_EVERYTHING` — preset
  to redact all Heap data on a component (deprecated/transitional, meant to
  be replaced by surgical per-attribute redaction);
  `getAnalyticsAttributes(name, options)` — builds `data-analytics-name` /
  `data-heap-redact-*` HTML attrs; `programmaticallyTraceError`.
- **React utilities:** `<Analytics name>` component — wraps/augments
  children with analytics HTML attributes (auto-detects if child is a DOM
  element, text, or component; wraps components in a `display:contents` div,
  or clones props onto DOM elements); `InitializeTelemetry` — no-render
  component wiring up telemetry listeners inside class components (used in
  `Main.js` entrypoint); `useGetAnalyticsAttributes` hook.
- **Custom events:** `trackCustomEvent(event, options)` — tracks a
  `"location - action - object"` named event in Heap (`window.heap.track`)
  and adds a matching Sentry breadcrumb.
- Also re-exports `GlobalWindowHeap` type and everything from `src/types.ts`
  (`HtmlAnalyticsAttributes`, `HtmlNameAttributes`).

## Providers

- **Heap** (`src/core/heap/*`) — consumed only via `window.heap`
  (optional/injected by server; direct access is marked `@deprecated` in
  types purely to nudge you through this module's abstractions instead).
  Handles user properties, custom event tracking, and data redaction
  (`data-heap-redact-text`, `data-heap-redact-attributes`).
- **Sentry** (`src/core/sentry/*`) — `startSentryTracing`,
  `captureException`, `getSentryEnvironment`, `parseSentryDsn`,
  `errorMustBeBlocked` (filters noisy/GraphiQL errors), breadcrumbs via
  `trackCustomEvent`.
- **Custom DOM-event telemetry** (`src/core/telemetry/htmlEvents.ts`) —
  listens for `click`/`change` on `document`, walks up via `closest()` to
  find `data-analytics-name`, forwards to a caller-supplied
  `UserEventTracker` — currently wired up only for ee-lite per an in-code
  comment.

## Gotchas

- `data-trackid` is legacy (`legacyTrackIdAttribute` option);
  `data-analytics-name` is current — ESLint's `react/forbid-dom-props` rule
  must be updated in tandem if these attribute names change.

## Dependencies on other `@hasura/shared/*` packages

`@hasura/shared/types` (`EnvVars`, used in `startTracing`), `@hasura/shared/ui`.

## Testing

Vitest via `@nx/vitest:test` (`nx test analytics`), jsdom environment,
shared setup file `../../../tools/test-setup/setupTests.ts`. Tests are
colocated `*.test.ts(x)` files alongside source (e.g. `Analytics.test.tsx`,
`getRedactAttributes.test.ts`).
