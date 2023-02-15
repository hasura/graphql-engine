import type { StorybookConfig } from '@storybook/core-common';
import { Configuration } from 'webpack';
import rootMain from '../../../../.storybook/main';

const config: StorybookConfig = {
  ...rootMain,

  core: { ...rootMain.core, builder: 'webpack5' },

  staticDirs: ['../../../../static'],

  stories: [
    ...rootMain.stories,
    '../src/lib/**/*.stories.mdx',
    '../src/lib/**/*.stories.@(js|jsx|ts|tsx)',
  ],

  addons: [...rootMain.addons, '@nrwl/react/plugins/storybook'],

  webpackFinal: async (config: Configuration) => {
    // apply any global webpack configs that might have been specified in .storybook/main.ts
    if (rootMain.webpackFinal) {
      config = await rootMain.webpackFinal(config);
    }

    return config;
  },
};

export default config;
