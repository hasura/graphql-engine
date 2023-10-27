import * as webpack from 'webpack';
import { merge } from 'webpack-merge';
export default {
  addons: [
    '@storybook/addon-links',
    '@storybook/addon-interactions',
    '@storybook/addon-a11y',
    '@storybook/addon-mdx-gfm',
  ],
  webpackFinal: async (config: any) => {
    const finalConfig = merge(config, {
      plugins: [
        new webpack.DefinePlugin({
          __CLIENT__: 'true',
          __SERVER__: false,
          __DEVELOPMENT__: true,
          __DEVTOOLS__: true,
          // <-------- DISABLE redux-devtools HERE
          CONSOLE_ASSET_VERSION: Date.now().toString(),
        }),
        // un comment this to test out the circular deps. Left here since it can be tricky to configure
      ],

      resolve: {
        fallback: {
          fs: false,
          os: false,
          http: false,
          https: require.resolve('https-browserify'),
          path: require.resolve('path-browserify'),
          crypto: false,
          util: require.resolve('util/'),
          stream: false,
          url: false,
        },
      },
    });
    finalConfig.module.rules = finalConfig.module.rules.map((rule: any) => {
      if (/source-map-loader/.test(rule.loader)) {
        return {
          ...rule,
          exclude: /node_modules/, // we don't want source maps for vendors, because of graphiql
        };
      }

      if (/file-loader/.test(rule.loader)) {
        return {
          ...rule,
          type: 'javascript/auto', // This is fixing issue https://webpack.js.org/guides/asset-modules/
        };
      }

      return rule;
    });
    return finalConfig;
  },
  framework: {
    name: '@storybook/react-webpack5',
    options: {},
  },
  docs: {
    autodocs: true,
  },
  parameters: {
    chromatic: {
      // Default value is 0.063
      diffThreshold: 0.4,
    },
  },
};
