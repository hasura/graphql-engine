// Webpack config for creating the production bundle.
const path = require('path');
const webpack = require('webpack');
const CleanPlugin = require('clean-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const hasuraConfig = require('../hasuraconfig');

const relativeAssetsPath = '../static/dist';
const assetsPath = path.join(__dirname, relativeAssetsPath);

const TerserPlugin = require('terser-webpack-plugin');
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const commonConfig = require('./common.config');

const cleanOptions = {
  root: process.cwd(),
  verbose: true,
  dry: false,
};

module.exports = {
  mode: 'production',
  context: path.resolve(__dirname, '..'),
  entry: {
    main: ['./src/client.js'],
  },
  output: {
    path: assetsPath,
    filename: '[name].js',
    chunkFilename: '[name].js',
    publicPath: hasuraConfig.webpackPrefix,
  },
  node: {
    module: 'empty',
    fs: 'empty',
    net: 'empty',
    child_process: 'empty',
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
          MiniCssExtractPlugin.loader,
          {
            loader: 'css-loader',
            options: {
              importLoaders: 2,
              sourceMap: false,
              modules: {},
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
              sourceMap: false,
              sourceMapContents: false,
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
  optimization: {
    minimize: true,
    runtimeChunk: {
      name: 'vendor',
    },
    minimizer: [
      new TerserPlugin({
        terserOptions: {
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
          parallel: 2,
        },
        extractComments: false,
      }),
      new OptimizeCssAssetsPlugin({
        assetNameRegExp: /\.css$/,
      }),
    ],
    splitChunks: {
      cacheGroups: {
        commons: {
          test: /[\\/]node_modules[\\/]/,
          name: 'vendor',
          chunks: 'initial',
        },
      },
    },
  },
  plugins: [
    new CleanPlugin(['./static/dist/*.*'], cleanOptions),

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
      CONSOLE_ASSET_VERSION: Date.now().toString(),
      'process.hrtime': () => null,
    }),
    new ForkTsCheckerWebpackPlugin({
      compilerOptions: {
        allowJs: true,
        checkJs: false,
      },
    }),
  ],
};
