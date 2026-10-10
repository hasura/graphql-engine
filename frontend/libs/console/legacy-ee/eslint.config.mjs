import baseConfig from '../../../eslint.config.mjs';

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
  {
    files: ['**/*.ts', '**/*.tsx', '**/*.js', '**/*.jsx'],
    rules: {
      'no-unused-vars': 'off',
      'react/jsx-key': 'off',
      'react-hooks/exhaustive-deps': 'off',
      'react-hooks/immutability': 'off',
      'react-hooks/set-state-in-effect': 'off',
      'react-hooks/purity': 'off',
      'react-hooks/refs': 'off',
      'react-hooks/set-state-in-render': 'off',
      'react-hooks/rules-of-hooks': 'off',
      'react/no-deprecated': 'off',
      'react/no-unknown-property': 'off',
      '@typescript-eslint/no-empty-function': 'off',
    },
  },
];
