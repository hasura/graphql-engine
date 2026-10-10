import { FlatCompat } from '@eslint/eslintrc';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import js from '@eslint/js';
import baseConfig from '../../../eslint.config.mjs';

const compat = new FlatCompat({
  baseDirectory: dirname(fileURLToPath(import.meta.url)),
  recommendedConfig: js.configs.recommended,
});

export default [
  ...baseConfig,
  {
    languageOptions: {
      globals: {
        __DEVELOPMENT__: true,
        __CLIENT__: true,
        __SERVER__: true,
        __DISABLE_SSR__: true,
        __DEVTOOLS__: true,
        socket: true,
        webpackIsomorphicTools: true,
        CONSOLE_ASSET_VERSION: true,
      },
    },
  },
  ...compat
    .config({
      extends: ['plugin:testing-library/react', 'plugin:jest-dom/recommended'],
    })
    .map((config) => ({
      ...config,
      files: ['**/__tests__/**/*.[jt]s?(x)', '**/?(*.)+(spec|test).[jt]s?(x)'],
      rules: {
        ...config.rules,
        'testing-library/no-unnecessary-act': 'warn',
        'testing-library/prefer-query-by-disappearance': 'warn',
        'testing-library/prefer-find-by': 'warn',
        'testing-library/prefer-presence-queries': 'warn',
        'testing-library/no-node-access': 'off',
      },
    })),
  {
    files: [
      '**/*.stories.@(ts|tsx|js|jsx|mjs|cjs)',
      '**/*.story.@(ts|tsx|js|jsx|mjs|cjs)',
    ],
    rules: {
      '@typescript-eslint/no-floating-promises': 'error',
    },
    languageOptions: {
      parserOptions: {
        project: [
          'libs/console/legacy-ce/tsconfig.*?.json',
          'libs/console/legacy-ce/.storybook/tsconfig.json',
        ],
      },
    },
  },
];
