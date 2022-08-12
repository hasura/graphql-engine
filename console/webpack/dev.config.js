// Webpack config for development
require('dotenv/config');
const fs = require('fs');
const path = require('path');
const webpack = require('webpack');
const assetsPath = path.resolve(__dirname, '../static/dist');
const hasuraConfig = require('../hasuraconfig');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { env } = require('../src/helpers/localDev');

const autoprefixer = require('autoprefixer');

const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const CaseSensitivePathsPlugin = require('case-sensitive-paths-webpack-plugin');

const commonConfig = require('./common.config');

module.exports = {
  mode: 'development',
  devtool: 'eval-source-map',
  context: path.resolve(__dirname, '..'),
  devServer: {
    port: hasuraConfig.port.development,
    historyApiFallback: true,
    client: {
      overlay: {
        warnings: false,
        errors: true,
      },
    },
  },
  node: {
    module: 'empty',
    fs: 'empty',
    net: 'empty',
    child_process: 'empty',
  },
  entry: {
    main: ['./src/client.js'],
  },
  output: {
    path: assetsPath,
    filename: '[name]-[hash].js',
    chunkFilename: '[name]-[chunkhash].js',
    publicPath: '/',
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
    new HtmlWebpackPlugin({
      title: 'Hasura Console',
      template: './public/index.html', // template file
      filename: 'index.html', // output file
      publicPath: '/',
      favicon: './static/favicon_green.png',
      env,
    }),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
    }),
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
      CONSOLE_ASSET_VERSION: Date.now().toString(),
      'process.hrtime': () => null,
    }),
    new ForkTsCheckerWebpackPlugin({
      compilerOptions: {
        allowJs: true,
        checkJs: false,
      },
    }),
    new CaseSensitivePathsPlugin(),
  ],
};
