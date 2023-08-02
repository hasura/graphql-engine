import type { StorybookConfig } from '@storybook/react-webpack5';
import { Configuration } from 'webpack';
import rootMain from '../../../../.storybook/main';

const config: StorybookConfig = {
  ...rootMain,

  staticDirs: ['../../../../static'],

  stories: [
    '../src/stories/**/*.stories.mdx',
    '../src/stories/**/*.stories.@(js|jsx|ts|tsx)',
  ],

  addons: [
    ...(rootMain.addons.filter(
      addon => addon !== 'storybook-addon-console-env'
    ) || []),
    '@nrwl/react/plugins/storybook',
    './../preset.js',
  ],

  webpackFinal: async (config: Configuration) => {
    // apply any global webpack configs that might have been specified in .storybook/main.ts
    if (rootMain.webpackFinal) {
      config = await rootMain.webpackFinal(config);
    }

    return config;
  },

  framework: {
    name: '@storybook/react-webpack5',
    options: {},
  },
};

export default config;
