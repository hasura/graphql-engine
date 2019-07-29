const path = require('path');
const webpack = require('webpack');
const CleanPlugin = require('clean-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
// const hasuraConfig = require('../hasuraconfig');

const relativeAssetsPath = '../packages/console-oss/build';
const assetsPath = path.join(__dirname, relativeAssetsPath);

const WebpackIsomorphicToolsPlugin = require('webpack-isomorphic-tools/plugin');
const webpackIsomorphicToolsPlugin = new WebpackIsomorphicToolsPlugin(
  require('./webpack-isomorphic-tools')
);

const UglifyJSPlugin = require('uglifyjs-webpack-plugin');
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const nodeExternals = require('webpack-node-externals');

// const BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;

const cleanOptions = {
  root: process.cwd(),
  verbose: true,
  dry: false,
};

module.exports = {
  externals: [nodeExternals()],
  mode: 'production',
  context: path.resolve(__dirname, '..'),
  entry: './pro-index.js',
  output: {
    path: assetsPath,
    filename: '[name].js',
    chunkFilename: '[name].js',
    libraryTarget: 'umd',
    library: 'console-oss',
  },
  module: {
    rules: [
      {
        test: /\.mjs$/,
        include: /node_modules/,
        type: 'javascript/auto',
      },
      {
        test: /\.jsx?$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            babelrc: false,
            presets: ['@babel/preset-env', '@babel/preset-react'],
            plugins: [
              '@babel/plugin-proposal-export-default-from',
              'transform-es2015-modules-commonjs',
              '@babel/plugin-proposal-class-properties',
            ],
          },
        },
      },
      {
        test: /\.flow$/,
        loader: 'ignore-loader',
      },
      {
        test: /\.css$/,
        use: [
          'style-loader',
          {
            loader: 'css-loader',
            options: {
              importLoaders: 1,
            },
          },
        ],
      },
      {
        test: /\.scss$/,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader?modules&importLoaders=2&sourceMap=false',
          'sass-loader?outputStyle=expanded&sourceMap=false&sourceMapContents=false',
        ],
      },
      {
        test: /\.woff(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: 'url-loader',
            options: { limit: 10000, mimetype: 'application/font-woff' },
          },
        ],
      },
      {
        test: /\.woff2(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: 'url-loader',
            options: { limit: 10000, mimetype: 'application/font-woff' },
          },
        ],
      },
      {
        test: /\.ttf(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: 'url-loader',
            options: { limit: 10000, mimetype: 'application/octet-stream' },
          },
        ],
      },
      {
        test: /\.eot(\?v=\d+\.\d+\.\d+)?$/,
        use: [{ loader: 'file-loader' }],
      },
      {
        test: /\.svg(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: 'url-loader',
            options: { limit: 1, mimetype: 'image/svg+xml' },
          },
        ],
      },
      {
        test: webpackIsomorphicToolsPlugin.regular_expression('images'),
        use: [{ loader: 'url-loader', options: { limit: 1 } }],
      },
    ],
  },
  resolve: {
    modules: ['src', 'node_modules'],
    extensions: ['.json', '.js', '.jsx', '.mjs'],
  },
  optimization: {
    minimize: true,
    splitChunks: {
      chunks: 'all',
    },
    minimizer: [
      new UglifyJSPlugin({
        uglifyOptions: {
          ecma: 8,
          warnings: false,
          compress: false,
          mangle: true,
          output: {
            comments: false,
            beautify: false,
          },
          toplevel: false,
          nameCache: null,
          ie8: false,
          keep_classnames: undefined,
          keep_fnames: false,
          safari10: false,
        },
      }),
      new OptimizeCssAssetsPlugin({
        assetNameRegExp: /\.css$/,
      }),
    ],
  },
  plugins: [
    new CleanPlugin(['./packages/console-oss/build/*.*'], cleanOptions),

    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
    }),
    new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      filename: 'main.css',
      chunkFilename: 'main.css',
    }),
    new webpack.DefinePlugin({
      __CLIENT__: true,
      __SERVER__: false,
      __DEVELOPMENT__: false,
      __DEVTOOLS__: false,
    }),

    // ignore dev config
    new webpack.IgnorePlugin(/\.\/dev/, /\/config$/),

    // set global consts
    new webpack.DefinePlugin({
      'process.env': {
        // Useful to reduce the size of client-side libraries, e.g. react
        NODE_ENV: JSON.stringify('production'),
      },
    }),
    // new BundleAnalyzerPlugin()
  ],
};
