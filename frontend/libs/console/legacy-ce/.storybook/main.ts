import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';
import type { StorybookConfig } from '@storybook/react-webpack5';
import rootMain from '../../../../.storybook/main';

// These options were migrated by @nx/storybook:convert-to-inferred from the project.json file.
const configValues = { default: {}, ci: {} };

// Determine the correct configValue to use based on the configuration
const nxConfiguration = process.env.NX_TASK_TARGET_CONFIGURATION ?? 'default';

const options = {
  ...configValues.default,
  ...(configValues[nxConfiguration] ?? {}),
};

const config: StorybookConfig = {
  ...rootMain,

  staticDirs: ['../../../../static'],

  stories: [
    '../src/lib/**/*.mdx',
    '../src/lib/**/*.stories.(js|jsx|ts|tsx)',
    '../../../shared/ui/src/**/*.stories.(js|jsx|ts|tsx)',
  ],

  addons: [
    ...(rootMain.addons ?? []),
    // Subpath export of the installed `@nx/react` package; the rule only
    // matches bare package names against package.json.

    '@nx/react/plugins/storybook',
    getAbsolutePath('@storybook/addon-mcp'),
  ],

  webpackFinal: async (config, opts) => {
    console.log('INIT webpack final');
    // apply any global webpack configs that might have been specified in .storybook/main.ts
    if (rootMain.webpackFinal) {
      config = await rootMain.webpackFinal(config, opts);
    }

    return config;
  },
  framework: {
    name: getAbsolutePath('@storybook/react-webpack5'),
    options,
  },
};

export default config;

function getAbsolutePath(value: string): any {
  return dirname(fileURLToPath(import.meta.resolve(`${value}/package.json`)));
}
