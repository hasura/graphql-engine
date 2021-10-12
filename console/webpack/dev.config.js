// Webpack config for development
const fs = require('fs');
const path = require('path');
const webpack = require('webpack');
const assetsPath = path.resolve(__dirname, '../static/dist');
const hasuraConfig = require('../hasuraconfig');
const host = hasuraConfig.hmrHost;
const port = hasuraConfig.hmrPort;

const autoprefixer = require('autoprefixer');

const WebpackIsomorphicToolsPlugin = require('webpack-isomorphic-tools/plugin');
const webpackIsomorphicToolsPlugin = new WebpackIsomorphicToolsPlugin(
  require('./webpack-isomorphic-tools')
);
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const CaseSensitivePathsPlugin = require('case-sensitive-paths-webpack-plugin');
const commonConfig = require('./common.config');

module.exports = {
  mode: 'development',
  devtool: 'inline-source-map',
  context: path.resolve(__dirname, '..'),
  node: {
    module: 'empty',
    fs: 'empty',
    net: 'empty',
    child_process: 'empty',
  },
  entry: {
    main: [
      `webpack-hot-middleware/client?path=http://${host}:${port}/__webpack_hmr`,
      'bootstrap-loader?extractStyles',
      'font-awesome-webpack!./src/theme/font-awesome.config.js',
      './src/client.js',
    ],
  },
  output: {
    path: assetsPath,
    filename: '[name]-[hash].js',
    chunkFilename: '[name]-[chunkhash].js',
    publicPath: `http://${host}:${port}${hasuraConfig.webpackPrefix}`,
  },
  module: {
    rules: [
      {
        test: /\.mjs$/,
        include: /node_modules/,
        type: 'javascript/auto',
      },
      {
        test: /\.(j|t)sx?$/,
        exclude: /node_modules/,
        use: 'babel-loader',
      },
      {
        test: /\.flow$/,
        loader: 'ignore-loader',
      },
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
      ...commonConfig.assetsRules,
    ],
  },
  resolve: {
    modules: ['src', 'node_modules'],
    extensions: ['.json', '.js', '.jsx', '.mjs', '.ts', '.tsx'],
    plugins: commonConfig.resolvePlugins,
  },
  plugins: [
    // hot reload
    // new UnusedFilesWebpackPlugin({}),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
    }),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.LoaderOptionsPlugin({
      postcss: [autoprefixer],
    }),
    // new BundleAnalyzerPlugin(),
    new webpack.IgnorePlugin(/webpack-stats\.json$/),
    new webpack.DefinePlugin({
      __CLIENT__: true,
      __SERVER__: false,
      __DEVELOPMENT__: true,
      __DEVTOOLS__: true, // <-------- DISABLE redux-devtools HERE
    }),
    // set global consts
    new webpack.DefinePlugin({
      CONSOLE_ASSET_VERSION: Date.now().toString(),
      'process.hrtime': () => null,
    }),
    webpackIsomorphicToolsPlugin.development(),
    new ForkTsCheckerWebpackPlugin({
      compilerOptions: {
        allowJs: true,
        checkJs: false,
      },
    }),
    new CaseSensitivePathsPlugin(),
  ],
};
