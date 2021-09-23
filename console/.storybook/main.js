const util = require('util');
const webpack = require('webpack');

module.exports = {
  features: {
    previewCsfV3: true,
  },
  stories: [
    '../src/**/*.stories.mdx',
    '../src/**/*.stories.@(js|jsx|ts|tsx|mdx)',
  ],
  babel: async options => {
    console.log('------BABEL--------');
    console.log(util.inspect(options, { showHidden: false, depth: null }));
    return options;
  },
  webpackFinal: async (config, { configType }) => {
    const newConfig = {
      ...config,
      entry: [
        'bootstrap-loader?extractStyles',
        'font-awesome-webpack!./src/theme/font-awesome.config.js',
        ...config.entry,
      ],
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
        new webpack.ProvidePlugin({
          $: 'jquery',
          jQuery: 'jquery',
        }),
        new webpack.DefinePlugin({
          CONSOLE_ASSET_VERSION: Date.now().toString(),
          'process.hrtime': () => null,
          __CLIENT__: true,
          __SERVER__: false,
          __DEVELOPMENT__: true,
          __DEVTOOLS__: true, // <-------- DISABLE redux-devtools HERE
        }),
      ],
    };

    console.log('------WEBPACK--------');
    console.log(util.inspect(newConfig, { showHidden: false, depth: null }));
    // console.log(util.inspect(config, { showHidden: false, depth: null }));

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
  ],
};
