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
      /*
      We can safely ignore warning from the source map loader if it can't parse source maps from node_modules
      worst case, some specific vendors source map may be wierd in dev
       */
      ignoreWarnings: [/Failed to parse source map/],
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
          // exclude: /node_modules/, // we don't want source maps for vendors, because of graphiql
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
