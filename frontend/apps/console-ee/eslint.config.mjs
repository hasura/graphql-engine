import baseConfig from '../../eslint.config.mjs';

export default [
  ...baseConfig,
  {
    ignores: ['apps/console-ee/src/assets/**'],
  },
];
