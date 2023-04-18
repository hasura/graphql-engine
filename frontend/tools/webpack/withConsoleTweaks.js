const { merge } = require('webpack-merge');
const webpack = require('webpack');
const util = require('util');

const log = value =>
  console.log(
    util.inspect(value, { showHidden: false, depth: null, colors: true })
  );

module.exports =
  () =>
  (config, { options, context }) => {
    const isDevBuild = context.configurationName === 'development';
    let finalConfig = merge(config, {
      output: {
        publicPath: 'auto',
      },
      plugins: [
        new webpack.DefinePlugin({
          __DEVELOPMENT__: context.configuration === 'development',
          CONSOLE_ASSET_VERSION: Date.now().toString(),
        }),
      ],
      resolve: {
        fallback: {
          /*
      Used by :
      openapi-to-graphql and it's deps (graphql-upload > fs-capacitor)
      no real polyfill exists, so this turns it into an empty implementation
       */
          fs: false,
          /*
      Used by :
      openapi-to-graphql and it's deps (graphql-upload > fs-capacitor)
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
        },
      },
    });

    if (!isDevBuild) {
      finalConfig = merge(finalConfig, {
        // Forcing webpack to not generate sourcemap by itself
        devtool: false,
        plugins: [
          // and instead fine tune the source map generation, excluding vendors
          new webpack.SourceMapDevToolPlugin({
            exclude: [/vendor\..*\.js/],
            filename: '[file].map',
          }),
        ],
      });
    }

    finalConfig.module.rules = finalConfig.module.rules.map(rule => {
      if (/source-map-loader/.test(rule.loader)) {
        return {
          ...rule,
          exclude: /node_modules/, // we don't want source maps for vendors, because of graphiql
        };
      }

      if (/file-loader/.test(rule.loader)) {
        return {
          ...rule,
          type: 'javascript/auto', // This is fixing issue https://webpack.js.org/guides/asset-modules/
        };
      }

      return rule;
    });

    // log(finalConfig.module);

    return finalConfig;
  };
