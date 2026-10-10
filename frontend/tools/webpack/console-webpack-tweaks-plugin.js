// @ts-check
// Not part of webpack's public API/types, but this is how webpack's own
// WebpackOptionsApply constructs it internally from `options.ignoreWarnings`.
const IgnoreWarningsPlugin = require('webpack/lib/IgnoreWarningsPlugin');
const { SourceMapDevToolPlugin } = require('webpack');
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin');
const {
  MonacoWorkerPublicPathPlugin,
} = require('./monaco-worker-public-path-plugin');

class ConsoleWebpackTweaksPlugin {
  apply(compiler) {
    const isDevBuild = compiler.options.mode === 'development';

    // GraphiQL 5's Monaco editor spawns classic web workers. Nx 23 defaults
    // `output.scriptType: 'module'` (while keeping `output.module: false`), which
    // makes webpack emit `import.meta` into every chunk's public-path runtime —
    // a syntax error inside a classic worker. Rewrite only the worker chunks'
    // public-path runtime to the classic `self.location` form.
    new MonacoWorkerPublicPathPlugin().apply(compiler);

    const definePlugin = compiler.options.plugins.find(
      (plugin) => plugin?.constructor.name === 'DefinePlugin',
    );
    if (definePlugin && typeof definePlugin !== 'function') {
      definePlugin.definitions = {
        ...(definePlugin.definitions || {}),
        __DEVELOPMENT__: JSON.stringify(isDevBuild),
        CONSOLE_ASSET_VERSION: JSON.stringify(Date.now().toString()),
      };
    }

    compiler.options.plugins = compiler.options.plugins.map((plugin) => {
      if (!plugin?.definitions?.['process.env']) {
        return plugin;
      }

      delete plugin.definitions['process.env']['NX_CLOUD_ACCESS_TOKEN'];
      return plugin;
    });

    // `@hasura/open-api-to-graphql` is mapped to its source, so the app's
    // `strict` type-check would also check that ported lib, which is written
    // against `strict: false` (and is type-checked under that config by its own
    // build). Drop its issues from the app's check. Nx pushes the checker onto
    // `compiler.options.plugins` and webpack applies it after this plugin, and
    // it only reads its options in `apply()`, so mutating them here works.
    const forkTsChecker = compiler.options.plugins.find(
      (plugin) => plugin?.constructor.name === 'ForkTsCheckerWebpackPlugin',
    );
    if (forkTsChecker && typeof forkTsChecker !== 'function') {
      const issue = forkTsChecker.options.issue ?? {};
      forkTsChecker.options.issue = {
        ...issue,
        exclude: [
          ...[issue.exclude ?? []].flat(),
          /** @param {{file?: string}} tsIssue */
          (tsIssue) =>
            /libs[\\/]open-api-to-graphql[\\/]/.test(tsIssue.file ?? ''),
        ],
      };
    }

    compiler.options.output.publicPath = 'auto';

    // Nx removed the executor's `deleteOutputPath`, so without this stale
    // hashed bundles from previous builds pile up in `dist/apps/*` and get
    // packaged by `build-server-assets`.
    compiler.options.output.clean = true;

    // Nx turns `sourceMap: true` into `devtool: 'source-map'`, which also
    // emits a map for the vendor chunk. We must not ship vendor source maps
    // (Sentry limitation, enforced by `validate-javascript-bundle-output`), so
    // generate maps for everything except the vendor chunk instead. Like
    // `devtool`, this is read by webpack after all plugins' `apply()` run.
    if (compiler.options.devtool === 'source-map') {
      compiler.options.devtool = false;
      new SourceMapDevToolPlugin({
        filename: '[file].map',
        exclude: [/vendor\..*\.js/],
      }).apply(compiler);
    }

    // `IgnoreWarningsPlugin` is normally constructed once by webpack itself
    // from `options.ignoreWarnings`, before any plugin's `apply()` runs, so
    // mutating `compiler.options.ignoreWarnings` here would have no effect.
    // Apply our own instance directly instead.
    new IgnoreWarningsPlugin([
      (warning) => /Failed to parse source map/.test(warning.message),
      /** @param {Error & {module?: {resource?: string}}} warning */
      (warning) =>
        /node_modules[\\/](react-datepicker|@readme[\\/]openapi-parser|monaco-editor)/.test(
          warning.module?.resource ?? '',
        ) &&
        // @readme/openapi-parser does the same for its Node-only
        // `node:dns/promises` and `undici` imports (used for URL safety
        // checks when resolving remote $refs), which never run in the browser.
        // react-datepicker guards its optional `date-fns-tz` require behind a
        // variable to avoid a hard crash when the package isn't installed
        // (https://github.com/Hacker0x01/react-datepicker/issues/6154), but
        // webpack can't statically resolve `require(variableName)` and always
        // flags it as a critical dependency, even though it resolves fine.
        /Critical dependency: the request of a dependency is an expression/.test(
          warning.message,
        ),
      // Monaco's CSS uses `justify-content: end`, which every browser in our
      // browserslist supports. It reaches us both from monaco-editor itself and
      // bundled inside `graphiql/style.css`, which postcss-import inlines into
      // GraphiQLWrapper/GraphiQL.css.
      /** @param {Error & {module?: {resource?: string}}} warning */
      (warning) =>
        /node_modules[\\/]monaco-editor|GraphiQLWrapper[\\/]GraphiQL\.css/.test(
          warning.module?.resource ?? '',
        ) &&
        /end value has mixed support, consider using flex-end instead/.test(
          warning.message,
        ),
    ]).apply(compiler);

    // Nx adds a default `CssMinimizerPlugin` (cssnano) for prod builds.
    // cssnano's `postcss-calc` pass can't parse `calc()` nested inside a
    // `var()` fallback, as used by `@radix-ui/themes` (leading-trim.css), and
    // fails with "postcss-calc:: Parse error". Disable just that pass.
    if (compiler.options.optimization?.minimizer) {
      compiler.options.optimization.minimizer =
        compiler.options.optimization.minimizer.map((minimizer) =>
          minimizer instanceof CssMinimizerPlugin
            ? new CssMinimizerPlugin({
                test: /\.(?:css|scss|sass|less)$/,
                minimizerOptions: {
                  preset: ['default', { calc: false }],
                },
              })
            : minimizer,
        );
    }

    compiler.options.module.rules = compiler.options.module.rules.map(
      (rule) => {
        if (
          rule &&
          typeof rule !== 'string' &&
          /source-map-loader/.test(rule.loader)
        ) {
          return {
            ...rule,
            exclude: /node_modules/, // we don't want source maps for vendors, because of graphiql
          };
        }

        return rule;
      },
    );

    // Mutate `resolve.fallback` in place rather than reassigning
    // `compiler.options.resolve` wholesale (e.g. via webpack-merge): other
    // plugins (like Nx's tsconfig-paths resolution) mutate the same
    // `resolve` object in place too, at various points during compiler
    // setup, and replacing the object risks dropping their changes
    // depending on plugin ordering.
    compiler.options.resolve.fallback = {
      ...compiler.options.resolve.fallback,
      /*
      Used by :
      openapi-to-graphql and it's deps
      no real polyfill exists, so this turns it into an empty implementation
       */
      fs: false,
      /*
      Used by :
      openapi-to-graphql and it's deps
       */
      os: require.resolve('os-browserify/browser'),
      /*
      Used by :
      openapi-to-graphql and it's deps (swagger2openapi)
       */
      http: require.resolve('stream-http'),
      /*
      Used by :
      @graphql-codegen/typescript and it's deps (@graphql-codegen/visitor-plugin-common && parse-filepath)
      => one usage is found, so we have to check if the usage is still relevant
       */
      path: require.resolve('path-browserify'),
      /*
      Used by :
      jsonwebtoken deps (jwa && jws)
      => we already have an equivalent in the codebases that don't depend on it,jwt-decode.
         Might be worth using only the latter
       */
      crypto: require.resolve('crypto-browserify'),
      /*
      Used by :
      jsonwebtoken deps (jwa && jws)
      @graphql-tools/merge => dependanci of graphiql & graphql-codegen/core, a package upgrade might fix it
       */
      util: require.resolve('util/'),
      /*
      Used by :
      jsonwebtoken deps (jwa && jws)
       */
      stream: require.resolve('stream-browserify'),
      https: require.resolve('https-browserify'),
      url: require.resolve('url/'),
      vm: require.resolve('vm-browserify'),
      zlib: false,
      tty: false,
      net: false,
      tls: false,
    };
  }
}

module.exports = { ConsoleWebpackTweaksPlugin };
