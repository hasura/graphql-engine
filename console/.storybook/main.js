const util = require('util');
const webpack = require('webpack');
const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');
const path = require('path');

const isConfigDebugMode = process.env.STORYBOOK_CONFIG_LOG === 'debug';

module.exports = {
  stories: ['../src/**/*.stories.mdx', '../src/**/*.stories.@(js|jsx|ts|tsx)'],
  babel: async options => {
    if (isConfigDebugMode) {
      console.log('------BABEL--------');
      console.log(util.inspect(options, { showHidden: false, depth: null }));
    }
    return options;
  },
  webpackFinal: async (config, rest) => {
    const newConfig = {
      ...config,
      module: {
        ...config.module,
        rules: [
          ...config.module.rules,
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
        ],
      },
      plugins: [
        ...config.plugins,
        new webpack.DefinePlugin({
          CONSOLE_ASSET_VERSION: Date.now().toString(),
          'process.hrtime': () => null,
          __CLIENT__: true,
          __SERVER__: false,
          __DEVELOPMENT__: true,
          __DEVTOOLS__: true, // <-------- DISABLE redux-devtools HERE
        }),
      ],
      resolve: {
        ...config.resolve,
        plugins: [
          ...config.resolve.plugins,
          new TsconfigPathsPlugin({
            configFile: path.resolve(__dirname, '../tsconfig.json'),
          }),
        ],
      },
    };

    if (isConfigDebugMode) {
      console.log('------WEBPACK--------');
      console.log(util.inspect(newConfig, { showHidden: false, depth: null }));
    }

    // Return the altered config
    return newConfig;
  },
  addons: [
    {
      name: '@storybook/addon-postcss',
      options: {
        postcssLoaderOptions: {
          implementation: require('postcss'),
        },
      },
    },
    '@storybook/addon-links',
    '@storybook/addon-essentials',
    '@storybook/addon-interactions'
  ],
};
