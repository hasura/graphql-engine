# AGENTS.md (apps/console-ee)

Nx project `console-ee` (EE / Pro / Cloud console). Thin webpack shell that
renders `Main` from `@hasura/console-legacy-ee` (`libs/console/legacy-ee/src/lib/client`),
which itself builds on `@hasura/console-legacy-ce`. Change UI in the libs, not
here. Structure mirrors `apps/console-ce` — read `apps/console-ce/AGENTS.md`
for the shared mechanics; this file lists what is the same and what differs.

## Layout / entry points

- `src/main.tsx` — `initAppearance()` then `createRoot(#content).render(<Main />)`.
- `src/index.html` — same `window.__env` builder as CE plus EE-only vars:
  server mode `NX_PUBLIC_HASURA_CLIENT_ID`, `NX_PUBLIC_HASURA_OAUTH_URL`,
  `NX_PUBLIC_HASURA_OAUTH_SCOPES`, `NX_PUBLIC_HASURA_CONSOLE_SENTRY_DSN`,
  `NX_PUBLIC_SSO_ENABLED`, `NX_PUBLIC_HASURA_SSO_PROVIDERS` (JSON-parsed);
  cli mode `NX_PUBLIC_PROJECT_ID`, `NX_PUBLIC_PROJECT_NAME`,
  `NX_PUBLIC_HASURA_METRICS_URL`, `NX_PUBLIC_IS_PRO`, Sentry DSN. Also loads
  `lottie.min.js` (loading animation) and Go `wasm_exec.js` from
  `graphql-engine-cdn.hasura.io` at runtime.
- `src/polyfills.ts` — CE's polyfills plus a `process.version` shim.
- `src/css/tailwind.css` — `@source` covers `libs/`, `apps/console-ce/src`
  and `apps/console-ee/src`.
- `src/assets/common/**` — CE's set plus `nodejs-azure` codegen, `js/lottie.min.js`,
  `wasm/go1.13|go1.16/wasm_exec.js`, github/twitter icons.

## Build & serve

Same as CE: `build`, `serve`, `preview`, `serve-static`, `build-deps`,
`watch-deps` are **inferred** by `@nx/webpack/plugin` from `webpack.config.js`
(`npx nx show project console-ee`); `project.json` only has `lint`, `test`,
`validate-javascript-bundle-output`, `build-server-assets`.

- `build` = `webpack-cli build` (cwd `apps/console-ee`, `NODE_ENV=production`,
  `-c development` available) → `dist/apps/console-ee`. ~1.5 min.
- `serve` = `webpack-cli serve`, port **5500**, `allowedHosts: 'all'`,
  `historyApiFallback`, HMR. No proxy.
- `webpack.config.js` is identical to CE's except: port/`allowedHosts`, paths,
  and `extractLicenses: isProd` (CE has `false`). Same `env.WEBPACK_SERVE`
  switches (`buildLibsFromSource`, `extractCss`) and `ConsoleWebpackTweaksPlugin`.
- `tsconfig.app.json` / `tsconfig.build.json` / `tsconfig.spec.json`,
  `postcss.config.js` (`@tailwindcss/postcss`), `.babelrc` — byte-identical to CE.

## Env vars / runtime config

Same dotenv loading as CE (`apps/console-ee/.env*`, then `frontend/.env*`;
`frontend/.env` is gitignored). For a local Pro/EE server set at least
`NX_PUBLIC_DATA_API_URL`, `NX_PUBLIC_CONSOLE_MODE=server` and
`NX_PUBLIC_HASURA_CONSOLE_TYPE=pro` (or `pro-lite`/`cloud`), plus the OAuth vars
above if testing login. `EnvVars` in `libs/shared/types/src/env.ts` is the
typed shape. Restart `serve` after editing `.env`.

## Server assets packaging

`build-server-assets` → `validate-javascript-bundle-output` → `build`, same
executors and rules as CE. Output: `dist/apps/server-assets-console-ee`
(`index.html`, `common/`, `versioned/` with gzipped entry assets,
`assetLoader.js.gz`, legacy `main.js.gz`).

## Testing

`test` = `@nx/vitest:test` (`vitest.config.mts`, jsdom, `passWithNoTests`);
no unit tests here. E2E: `apps/console-ee-e2e` starts
`npx nx run console-ee:serve` and waits on `http://localhost:5500`.

## Gotchas

- `lint` has `"fix": true` in `project.json` — running it **rewrites files**
  (CE's lint does not). Check `git diff` afterwards.
- Runtime CDN scripts in `index.html` mean the loading animation / wasm features
  fail offline; that is not a build problem.
- Keep `webpack.config.js` and `src/css/tailwind.css` in sync with CE when
  changing shared build behaviour.
- Don't package a `-c development` build (unhashed `main.js` fails validation).

## Checks before finishing

Run from `frontend/` (Node 24, `.nvmrc`):

```sh
npx nx run console-ee:lint                  # auto-fixes; review git diff
npx nx run console-ee:test
npx nx run console-ee:build-server-assets   # runs build + validator too
yarn start:ee                               # if serve/dev config changed; open http://localhost:5500
```
