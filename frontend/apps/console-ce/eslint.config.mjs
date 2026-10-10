import baseConfig from '../../eslint.config.mjs';

export default [
  ...baseConfig,
  {
    ignores: ['apps/console-ce/src/assets/**'],
  },
];
