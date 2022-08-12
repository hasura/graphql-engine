const util = require('util');
const webpack = require('webpack');
const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');
const path = require('path');

const isConfigDebugMode = process.env.STORYBOOK_CONFIG_LOG === 'debug';

module.exports = {
  stories: ['../src/*.mdx', '../src/**/*.stories.@(js|jsx|ts|tsx|mdx)'],
  babel: async options => {
    if (isConfigDebugMode) {
      console.log('------BABEL--------');
      console.log(util.inspect(options, { showHidden: false, depth: null }));
    }
    return options;
  },
  webpackFinal: async config => {
    config.module.rules.push(
      {
        test: /\.scss$/,
        use: [
          'style-loader',
          {
            loader: 'css-loader',
            options: {
              importLoaders: 2,
              modules: {
                localIdentName: '[local]___[hash:base64:5]',
              },
            },
          },
          {
            loader: 'sass-loader',
            options: {
              // Prefer `dart-sass`
              implementation: require('sass'),
              sassOptions: {
                outputStyle: 'expanded',
              },
              sourceMap: true,
            },
          },
        ],
      },
      {
        test: /\.mjs$/,
        include: /node_modules/,
        type: 'javascript/auto',
      }
    );
    config.plugins.push(
      new webpack.DefinePlugin({
        CONSOLE_ASSET_VERSION: Date.now().toString(),
        'process.hrtime': () => null,
        __CLIENT__: true,
        __SERVER__: false,
        __DEVELOPMENT__: true,
        __DEVTOOLS__: true, // <-------- DISABLE redux-devtools HERE
      })
    );
    config.resolve.plugins.push(
      new TsconfigPathsPlugin({
        configFile: path.resolve(__dirname, '../tsconfig.json'),
      })
    );
    config.resolve.alias['@'] = path.resolve(__dirname, '../src');

    if (isConfigDebugMode) {
      console.log('------WEBPACK--------');
      console.log(util.inspect(newConfig, { showHidden: false, depth: null }));
    }

    // Return the altered config
    return config;
  },
  addons: [
    {
      name: '@storybook/addon-postcss',
      options: {
        postcssLoaderOptions: {
          implementation: require('postcss'),
          postcssOptions: {
            config: path.resolve(__dirname, '../postcss-storybook.config.js'),
          },
        },
      },
    },
    '@storybook/addon-docs',
    '@storybook/addon-links',
    '@storybook/addon-essentials',
    '@storybook/addon-interactions',
    'storybook-dark-mode/register',
  ],
  features: {
    interactionsDebugger: true,
    babelModeV7: true,
  },
};
