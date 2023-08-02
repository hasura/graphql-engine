const { merge } = require('webpack-merge');
const dynamicAssetLoader = require('unplugin-dynamic-asset-loader');

module.exports =
  () =>
  (config, { options, context }) => {
    const isDevBuild = context.configurationName === 'development';
    if (!isDevBuild) {
      return config;
    }
    return merge(config, {
      plugins: [dynamicAssetLoader.webpackPlugin()],
    });
  };
