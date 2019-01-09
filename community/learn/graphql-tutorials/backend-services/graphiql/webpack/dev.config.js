// require('babel-polyfill');

// Webpack config for development
const fs = require('fs');
const path = require('path');
const webpack = require('webpack');
const assetsPath = path.resolve(__dirname, '../static/dist');
const appconfig = require('../appconfig');
const host = appconfig.hmrHost;
const port = appconfig.hmrPort;

const autoprefixer = require('autoprefixer');

const WebpackIsomorphicToolsPlugin = require('webpack-isomorphic-tools/plugin');
const webpackIsomorphicToolsPlugin = new WebpackIsomorphicToolsPlugin(
  require('./webpack-isomorphic-tools')
);

// const { UnusedFilesWebpackPlugin } = require('unused-files-webpack-plugin');

const babelrc = fs.readFileSync('./.babelrc');
let babelrcObject = {};

try {
  babelrcObject = JSON.parse(babelrc);
} catch (err) {
  console.error('==>     ERROR: Error parsing your .babelrc.');
  console.error(err);
}

const babelrcObjectDevelopment =
  (babelrcObject.env && babelrcObject.env.development) || {};
const babelLoaderQuery = Object.assign(
  {},
  babelrcObject,
  babelrcObjectDevelopment
);
delete babelLoaderQuery.env;

babelLoaderQuery.plugins = babelLoaderQuery.plugins || [];
if (babelLoaderQuery.plugins.indexOf('react-transform') < 0) {
  babelLoaderQuery.plugins.push('react-transform');
}

babelLoaderQuery.extra = babelLoaderQuery.extra || {};
if (!babelLoaderQuery.extra['react-transform']) {
  babelLoaderQuery.extra['react-transform'] = {};
}
if (!babelLoaderQuery.extra['react-transform'].transforms) {
  babelLoaderQuery.extra['react-transform'].transforms = [];
}
babelLoaderQuery.extra['react-transform'].transforms.push({
  transform: 'react-transform-hmr',
  imports: ['react'],
  locals: ['module'],
});

module.exports = {
  mode: 'development',
  devtool: 'inline-source-map',
  context: path.resolve(__dirname, '..'),
  entry: {
    main: [
      'webpack-hot-middleware/client?path=http://' +
        host +
        ':' +
        port +
        '/__webpack_hmr',
      'bootstrap-loader?extractStyles',
      'font-awesome-webpack!./src/theme/font-awesome.config.js',
      './src/client.js',
    ],
  },
  output: {
    path: assetsPath,
    filename: '[name]-[hash].js',
    chunkFilename: '[name]-[chunkhash].js',
    publicPath: 'http://' + host + ':' + port + appconfig.webpackPrefix,
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
        use: 'babel-loader',
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
          'style-loader',
          'css-loader?modules&importLoaders=2&sourceMap&localIdentName=[local]___[hash:base64:5]',
          'sass-loader?outputStyle=expanded&sourceMap',
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
            options: { limit: 10000, mimetype: 'image/svg+xml' },
          },
        ],
      },
      {
        test: webpackIsomorphicToolsPlugin.regular_expression('images'),
        use: [{ loader: 'url-loader', options: { limit: 10240 } }],
      },
    ],
  },
  resolve: {
    modules: ['src', 'node_modules'],
    extensions: ['.json', '.js', '.jsx', '.mjs'],
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
    new webpack.IgnorePlugin(/webpack-stats\.json$/),
    new webpack.DefinePlugin({
      __CLIENT__: true,
      __SERVER__: false,
      __DEVELOPMENT__: true,
      __DEVTOOLS__: true, // <-------- DISABLE redux-devtools HERE
    }),
    webpackIsomorphicToolsPlugin.development(),
  ],
};
