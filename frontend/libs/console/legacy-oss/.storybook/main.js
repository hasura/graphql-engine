const webpack = require('webpack');
const rootMain = require('../../../../.storybook/main');

module.exports = {
  ...rootMain,

  core: { ...rootMain.core, builder: 'webpack5' },

  staticDirs: ['../../../../static'],

  stories: [
    ...rootMain.stories,
    '../src/lib/**/*.stories.mdx',
    '../src/lib/**/*.stories.@(js|jsx|ts|tsx)',
  ],

  addons: [...rootMain.addons, '@nrwl/react/plugins/storybook'],

  webpackFinal: async (config, { configType }) => {
    // apply any global webpack configs that might have been specified in .storybook/main.js
    if (rootMain.webpackFinal) {
      config = await rootMain.webpackFinal(config, { configType });
    }

    config.plugins.push(
      new webpack.DefinePlugin({
        CONSOLE_ASSET_VERSION: Date.now().toString(),
        // 'process.hrtime': () => null,
        __CLIENT__: true,
        __SERVER__: false,
        __DEVELOPMENT__: true,
        __DEVTOOLS__: true, // <-------- DISABLE redux-devtools HERE
      })
    );

    return config;
  },
};
