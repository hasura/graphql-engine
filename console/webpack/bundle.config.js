const path = require('path');
const webpack = require('webpack');
const CleanPlugin = require('clean-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

const relativeAssetsPath = '../lib';
const assetsPath = path.join(__dirname, relativeAssetsPath);

const commonConfig = require('./common.config');

const TerserPlugin = require('terser-webpack-plugin');
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const nodeExternals = require('webpack-node-externals');

const getRandomHexString = () => {
  return Math.random().toString(16).slice(2);
};

const cleanOptions = {
  root: process.cwd(),
  verbose: true,
  dry: false,
};

const webpackConfiguration = {
  externals: [nodeExternals()],
  mode: 'production',
  context: path.resolve(__dirname, '..'),
  entry: {
    main: './exports/main.js',
    hoc: './exports',
    app: './exports/app',
    appState: './exports/appState',
    constants: './exports/constants',
  },
  output: {
    // Required to allow requiring modules bundled by nodejs apps
    // Ref: https://webpack.js.org/configuration/output/#outputglobalobject
    globalObject: 'this',
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
        test: /\.(j|t)sx?$/,
        exclude: /node_modules/,
        // use: 'babel-loader',
        use: {
          loader: 'babel-loader',
          options: {
            babelrc: false,
            presets: [
              '@babel/preset-env',
              '@babel/preset-react',
              '@babel/preset-typescript',
            ],
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
        },
        extractComments: false,
      }),
      new OptimizeCssAssetsPlugin({
        assetNameRegExp: /\.css$/,
      }),
    ],
  },
  stats: {
    errorDetails: true,
  },
  plugins: [
    new CleanPlugin(['./lib/*.*'], cleanOptions),

    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
    }),
    new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      filename: '[name].css',
      chunkFilename: '[name].css',
      // filename: 'main.css',
      // chunkFilename: 'main.css',
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
      'process.hrtime': () => null,
    }),
    new webpack.DefinePlugin({
      CONSOLE_ASSET_VERSION: JSON.stringify(getRandomHexString()),
    }),

    new ForkTsCheckerWebpackPlugin({
      compilerOptions: {
        allowJs: true,
        checkJs: false,
      },
    }),
  ],
};

if (process.env.CONSOLE_BUILD_GENERATE_SOURCE_MAPS === 'enabled') {
  // Until NX is a thing (that simplify the dependency graph of the Pro/Cloud Console) generating
  // source maps is a resource-intensive task that must be enabled only for the production-oriented
  // CI pipelines (not the PR-oriented pipelines, for instance).
  webpackConfiguration.devtool = 'source-map';

  // source-map-loader allows exporting the source maps of the dependencies to the final bundle.
  // This is theoretically needed only for the Pro Console (whose depends on the OSS Console) but in
  // order to work properly, it must be enabled for both the OSS and the Pro Console.
  webpackConfiguration.module.rules = [
    {
      test: /\.(j|t)sx?$/,
      enforce: 'pre',
      use: ['source-map-loader'],
    },
    ...webpackConfiguration.module.rules,
  ];
}

module.exports = webpackConfiguration;
