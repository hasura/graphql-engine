# AGENTS.md (apps/console-ce)

Nx project `console-ce` (CE / OSS console). Thin webpack shell — almost no UI
code lives here; it renders `ConsoleCeApp` (= `App` from
`libs/console/legacy-ce/src/lib/client.tsx`). Change UI in
`libs/console/legacy-ce`, not here. See `frontend/AGENTS.md` for workspace-wide info.

## Layout / entry points

- `src/main.tsx` — calls `initAppearance()` (`@hasura/shared/ui`, light/dark
  before first render) then `createRoot(#content).render(<ConsoleCeApp />)`.
- `src/index.html` — inline `<script>` builds `window.__env` from
  `%NX_PUBLIC_*%` placeholders (server vs cli shape picked by
  `NX_PUBLIC_CONSOLE_MODE`); `getEnv()` turns unreplaced `%NX…` into `undefined`.
- `src/polyfills.ts` — core-js, `Buffer`, `window.global`, `process.cwd` shim.
- `src/css/tailwind.css` — the only global `styles` entry (Tailwind v4, dark
  variant, `@source` dirs, GraphiQL fixes). `src/css/legacy-boostrap.css` is unreferenced.
- `src/assets/common/**` → `dist/apps/console-ce/common`; `src/environments/*`
  swapped via `fileReplacements` when `NODE_ENV=production`.

## Build & serve

`project.json` only declares `lint`, `test`, `validate-javascript-bundle-output`,
`build-server-assets`. **`build`, `serve`, `preview`, `serve-static`,
`build-deps`, `watch-deps` are inferred** by `@nx/webpack/plugin` (nx.json)
from `webpack.config.js` — inspect with `npx nx show project console-ce`.

- `build` = `webpack-cli build` (cwd `apps/console-ce`, `NODE_ENV=production`;
  `-c development` → `NODE_ENV=development`) → `dist/apps/console-ce`, ~1.5 min.
  `^build` first builds the `@nx/js:tsc` libs (shared-*, metadata-helpers, open-api-to-graphql).
- `serve` = `webpack-cli serve`, `NODE_ENV=development`, port **4200**,
  `historyApiFallback`, HMR. No dev-server proxy.
- `webpack.config.js`: `NxAppWebpackPlugin` (babel, `tsConfig: tsconfig.build.json`)
  - `NxReactWebpackPlugin` (SVGR) + `ConsoleWebpackTweaksPlugin`
    (`tools/webpack/console-webpack-tweaks-plugin.js`: `__DEVELOPMENT__`/
    `CONSOLE_ASSET_VERSION` defines, `output.clean`, `publicPath: 'auto'`, no
    vendor source maps, Node polyfill `resolve.fallback`s, Monaco worker
    public-path fix, ignores open-api-to-graphql type issues, cssnano `calc:false`).
- Mode switches use `env.WEBPACK_SERVE` (set only by `webpack serve`):
  `buildLibsFromSource` true for serve / false for build; `extractCss` false for
  serve (HMR) / true for build (server-assets loader needs `styles.css`).
- `output.path` must mirror `outputPath` or Nx caches nothing for `build`.
- `tsconfig.app.json` = app TS settings; `tsconfig.build.json` extends it and
  replaces `paths` (all `@hasura/*` → lib `src/index.ts`, no `@hasura/shared/testing`).
  Only `tsconfig.build.json` is fed to webpack (resolution + ForkTsChecker).
- `postcss.config.js` → `@tailwindcss/postcss` only.

## Env vars / runtime config

Nx loads dotenv files per task: `apps/console-ce/.env*` then `frontend/.env*`
(also `.build.env`, `.env.<target>`, etc.). `frontend/.env` is gitignored —
create it locally. `NX_PUBLIC_*` vars are interpolated into `index.html`
(`%NX_PUBLIC_X%`) and exposed as `process.env.NX_PUBLIC_*`. To point the dev
server at a local graphql-engine set `NX_PUBLIC_DATA_API_URL=http://localhost:8080`,
`NX_PUBLIC_CONSOLE_MODE=server`, `NX_PUBLIC_HASURA_CONSOLE_TYPE=oss` (full
list in `frontend/docs/generic-info.md`). Committed `frontend/.build.env` sets
`NODE_OPTIONS=--max-old-space-size=13312` for `build`. Restart `serve` after editing `.env`.

## Server assets packaging

`build-server-assets` (`libs/nx/internal-plugin/src/executors/build-server-assets`)
← `validate-javascript-bundle-output` ← `build`. Validator fails on
`NX_CLOUD_ACCESS_TOKEN` in the bundle, `vendor.*.js.map`, or files named
`main.js`/`main.css`/`vendor.js`/`assetLoader.js` — so it needs the hashed
production build. Packager copies `dist/apps/console-ce` →
`dist/apps/server-assets-console-ce`, moves files into `versioned/`, writes
`versioned/assetLoader.js` + legacy `versioned/main.js`, gzips entry assets.

## Testing

`test` = `@nx/vitest:test` (`vitest.config.mts`, jsdom, `passWithNoTests`).
No unit tests in this app. E2E: `apps/console-ce-e2e` starts
`npx nx run console-ce:serve` and waits on `http://localhost:4200` (5 min timeout).

## Gotchas

- Don't run `build-server-assets` against `build -c development` (unhashed `main.js` trips the validator).
- Any CSS dir Tailwind must scan needs an `@source` in `src/css/tailwind.css`
  (webpack-cli runs with cwd `apps/console-ce`, so auto-detection misses `libs/`).
- `yarn start:ce` adds `NODE_OPTIONS=--max-old-space-size=8192`; prefer it
  over bare `nx run console-ce:serve` (`.build.env` only applies to `build`).
- `serve` still runs `^build` first (nx.json `targetDefaults`), even though it
  compiles libs from source.
- Keep `webpack.config.js` in sync with `apps/console-ee`.

## Checks before finishing

Run from `frontend/` (Node 24, `.nvmrc`):

```sh
npx nx run console-ce:lint
npx nx run console-ce:test
npx nx run console-ce:build-server-assets   # runs build + validator too
yarn start:ce                               # if serve/dev config changed; open http://localhost:4200
```
