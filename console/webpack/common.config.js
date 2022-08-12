const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');
const path = require('path');

module.exports = {
  resolvePlugins: [
    new TsconfigPathsPlugin({
      configFile: path.resolve(__dirname, '..', 'tsconfig.json'),
    }),
  ],
  assetsRules: [
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
        {
          loader: 'postcss-loader',
          options: {
            postcssOptions: {
              config: true,
            },
          },
        },
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
      test: /\.(jpeg|jpg|png|gif)$/,
      use: [{ loader: 'url-loader', options: { limit: 10240 } }],
    },
  ],
};
