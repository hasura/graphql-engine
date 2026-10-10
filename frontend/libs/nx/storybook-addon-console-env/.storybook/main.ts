import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';
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
      (addon) => addon !== 'storybook-addon-console-env',
    ) || []),
    '@nx/react/plugins/storybook',
    './../preset.js',
    getAbsolutePath('@storybook/addon-mcp'),
  ],

  webpackFinal: async (config: Configuration) => {
    // apply any global webpack configs that might have been specified in .storybook/main.ts
    if (rootMain.webpackFinal) {
      config = await rootMain.webpackFinal(config);
    }

    return config;
  },

  framework: {
    name: getAbsolutePath('@storybook/react-webpack5'),
    options: {},
  },
};

export default config;

function getAbsolutePath(value: string): any {
  return dirname(fileURLToPath(import.meta.resolve(`${value}/package.json`)));
}
