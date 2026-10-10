import baseConfig from '../../../eslint.config.mjs';

export default [
  ...baseConfig,
  {
    files: ['**/*.ts', '**/*.tsx', '**/*.js', '**/*.jsx'],
    rules: {
      // Components in this library type their props via TypeScript, so the
      // runtime `react/prop-types` check is redundant and reports false
      // positives on TS-typed components. TypeScript already validates props.
      'react/prop-types': 'off',
    },
  },
];
