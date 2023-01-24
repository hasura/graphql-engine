const webpack = require('webpack');
const { merge } = require('webpack-merge');
const CircularDependencyPlugin = require('circular-dependency-plugin');

module.exports = {
  stories: [],
  addons: [
    '@storybook/addon-essentials',
    '@storybook/addon-links',
    '@storybook/addon-interactions',
    'storybook-dark-mode/register',
  ],
  webpackFinal: async (config, { configType }) => {
    return merge(config, {
      plugins: [
        new webpack.DefinePlugin({
          __CLIENT__: 'true',
          __SERVER__: false,
          __DEVELOPMENT__: true,
          __DEVTOOLS__: true, // <-------- DISABLE redux-devtools HERE
          CONSOLE_ASSET_VERSION: Date.now().toString(),
        }),
        // un comment this to test out the circular deps. Left here since it can be tricky to configure
      ],
      resolve: {
        fallback: {
          fs: false,
          os: false,
          http: false,
          path: require.resolve('path-browserify'),
          crypto: false,
          util: require.resolve('util/'),
          stream: false,
        },
      },
    });
  },
};
