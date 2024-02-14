import type { StorybookConfig } from '@storybook/react-webpack5';
import { Configuration } from 'webpack';
import rootMain from '../../../../.storybook/main';

const config: StorybookConfig = {
  ...rootMain,

  staticDirs: ['../../../../static'],

  stories: [
    '../src/lib/**/*.stories.mdx',
    '../src/lib/**/*.stories.(js|jsx|ts|tsx)',
  ],

  addons: [
    '@storybook/addon-essentials',
    ...rootMain.addons,
    '@nrwl/react/plugins/storybook',
  ],

  webpackFinal: async (config: Configuration) => {
    console.log('INIT webpack final');
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
