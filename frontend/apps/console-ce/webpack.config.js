// @ts-check
const { join } = require('path');
const { NxReactWebpackPlugin } = require('@nx/react/webpack-plugin');
const { NxAppWebpackPlugin } = require('@nx/webpack/app-plugin');
const {
  ConsoleWebpackTweaksPlugin,
} = require('../../tools/webpack/console-webpack-tweaks-plugin');

const isProd = process.env.NODE_ENV === 'production';

/**
 * @type{import('webpack').WebpackOptionsNormalized}
 */
module.exports = async (env = {}) => ({
  // Must mirror NxAppWebpackPlugin's `outputPath`: the @nx/webpack inference
  // plugin derives the `build` target's cache `outputs` from `output.path`
  // only. Without it `outputs` is empty, so a cache hit restores nothing and
  // dist/apps/console-ce is never produced.
  output: {
    path: join(__dirname, '../../dist/apps/console-ce'),
  },
  devServer: {
    // Client-side routing (react-router) needs unknown paths to fall back
    // to index.html rather than 404ing.
    historyApiFallback: true,
    hot: true,
    port: 4200,
  },
  plugins: [
    new NxAppWebpackPlugin({
      compiler: 'babel',
      babelUpwardRootMode: true,
      target: 'web',
      main: 'apps/console-ce/src/main.tsx',
      polyfills: 'apps/console-ce/src/polyfills.ts',
      tsConfig: 'apps/console-ce/tsconfig.build.json',
      outputPath: 'dist/apps/console-ce',
      index: 'apps/console-ce/src/index.html',
      baseHref: '/',
      // Dev server (Cypress / `webpack serve`) compiles workspace libraries
      // from source via tsconfig paths — the normal dev-serve/HMR path, which
      // also avoids the buildable-lib `dist/libs/*` resolution that fails for
      // transitive `@hasura/*` imports. Production asset builds keep `false`
      // (resolve the pre-built dist outputs). `WEBPACK_SERVE` is set only by
      // `webpack serve`.
      buildLibsFromSource: !!env.WEBPACK_SERVE,
      assets: [
        {
          glob: '**',
          input: 'apps/console-ce/src/assets/common',
          output: 'common',
        },
      ],
      styles: ['apps/console-ce/src/css/tailwind.css'],
      scripts: [],
      postcssConfig: 'apps/console-ce/postcss.config.js',
      fileReplacements: isProd
        ? [
            {
              replace: 'apps/console-ce/src/environments/environment.ts',
              with: 'apps/console-ce/src/environments/environment.prod.ts',
            },
          ]
        : [],
      // These three all have plugin defaults that are the opposite of what
      // console-ce needs for at least one of dev/prod, so they must stay
      // explicit rather than being left to infer from NODE_ENV.
      outputHashing: isProd ? 'bundles' : 'none',
      namedChunks: isProd,
      vendorChunk: isProd,
      // Enabled for both dev and prod so webpack-dev-server debugging and
      // prod error reporting both work against original sources.
      sourceMap: true,
      // Must stay on for builds: `build-server-assets` loads the console
      // through a generated asset loader that requires `styles.css` to be
      // linked from index.html. (Nx 15 always extracted the global `styles`
      // entry; newer Nx only does so when `extractCss` is set.)
      // Off for the dev server, where extracted CSS breaks HMR ("Loading CSS
      // chunk main failed") and style-loader hot-reloads CSS natively.
      // webpack-cli passes `WEBPACK_SERVE` in `env` only for `webpack serve`.
      extractCss: !env.WEBPACK_SERVE,
      extractLicenses: false,
      optimization: isProd,
      generateIndexHtml: true,
    }),
    new NxReactWebpackPlugin({
      // Uncomment this line if you don't want to use SVGR
      // See: https://react-svgr.com/
      // svgr: false,
    }),
    new ConsoleWebpackTweaksPlugin(),
  ],
});
