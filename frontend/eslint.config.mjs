import storybook from 'eslint-plugin-storybook';
import nx from '@nx/eslint-plugin';
import react from 'eslint-plugin-react';
import reactHooks from 'eslint-plugin-react-hooks';
import eslintConfigPrettier from 'eslint-config-prettier/flat';

export default [
  ...nx.configs['flat/base'],
  ...storybook.configs['flat/recommended'],
  eslintConfigPrettier,
  reactHooks.configs.flat.recommended,
  react.configs.flat['jsx-runtime'],
  {
    files: ['**/*.js', '**/*.jsx', '**/*.ts', '**/*.tsx'],
    ...react.configs.flat.recommended,
    rules: {
      ...react.configs.flat.recommended.rules,
      'react-hooks/set-state-in-effect': 'warn',
      'react/react-in-jsx-scope': 'off',
      'react/display-name': 'off',
      'react/no-unknown-property': 'off',
      'react/jsx-no-useless-fragment': [
        'warn',
        {
          allowExpressions: true,
        },
      ],
      'react/forbid-dom-props': [
        'error',
        {
          forbid: [
            {
              propName: 'data-analytics-name',
              message:
                'Analytics attributes (data-analytics-name) should be added through the Analytics component/utilities',
            },
            {
              propName: 'data-trackid',
              message:
                'Analytics attributes (data-trackid) should be added through the Analytics component/utilities',
            },
            {
              propName: 'data-heap-redact-text',
              message:
                'Analytics attributes (data-heap-redact-text) should be added through the Analytics component/utilities',
            },
            {
              propName: 'data-heap-redact-attributes',
              message:
                'Analytics attributes (data-heap-redact-attributes) should be added through the Analytics component/utilities',
            },
          ],
        },
      ],
    },
  },
  {
    files: [
      './apps/**/*.ts',
      './libs/console/**/*.ts',
      './libs/console/**/*.tsx',
    ],
    ignores: ['**/.storybook/**/*.ts'],
    rules: {
      '@nx/enforce-module-boundaries': [
        'error',
        {
          enforceBuildableLibDependency: true,
          allow: [],
          depConstraints: [
            {
              sourceTag: 'scope:shared',
              onlyDependOnLibsWithTags: ['scope:shared'],
            },
            {
              sourceTag: 'scope:console',
              onlyDependOnLibsWithTags: [
                'scope:console',
                'scope:shared',
                // The console consumes the metadata data-access libraries
                // (@hasura/metadata/*), which are tagged scope:metadata.
                'scope:metadata',
              ],
            },
            {
              sourceTag: 'scope:nx-plugins',
              onlyDependOnLibsWithTags: ['scope:shared', 'scope:nx-plugins'],
            },
            {
              sourceTag: 'type:utils',
              onlyDependOnLibsWithTags: ['type:utils'],
            },
            {
              sourceTag: 'type:data',
              onlyDependOnLibsWithTags: ['type:data', 'type:utils'],
            },
            {
              sourceTag: 'type:ui',
              onlyDependOnLibsWithTags: ['type:ui', 'type:utils'],
            },
            {
              sourceTag: 'type:feature',
              onlyDependOnLibsWithTags: [
                'type:data',
                'type:feature',
                'type:ui',
                'type:utils',
                // Shared + metadata libraries are tagged by scope only (no
                // `type:*`); the scope:* rules already permit depending on them, so
                // mirror that here instead of rejecting every @hasura/shared/* or
                // @hasura/metadata/* import.
                'scope:shared',
                'scope:metadata',
              ],
            },
            {
              sourceTag: 'type:app',
              onlyDependOnLibsWithTags: [
                'type:data',
                'type:utils',
                'type:ui',
                'type:feature',
              ],
            },
            {
              sourceTag: 'type:storybook',
              onlyDependOnLibsWithTags: [
                'type:data',
                'type:storybook',
                'type:utils',
                'type:feature',
                'type:ui',
              ],
            },
            {
              sourceTag: 'type:e2e',
              onlyDependOnLibsWithTags: [
                'type:data',
                'type:e2e',
                'type:app',
                'type:ui',
                'type:storybook',
                'type:feature',
                'type:utils',
                // Shared + metadata libraries are tagged by scope only (no `type:*`),
                // so allow e2e specs to use their contracts/helpers (e.g.
                // @hasura/shared/types, @hasura/metadata/api).
                'scope:shared',
                'scope:metadata',
              ],
            },
          ],
        },
      ],
    },
  },
  ...nx.configs['flat/typescript'],
  {
    files: ['**/*.ts', '**/*.tsx'],
    rules: {
      'no-unused-vars': 'off',
      'no-useless-escape': 'warn',
      '@typescript-eslint/no-unused-vars': [
        'error',
        {
          args: 'none',
          caughtErrors: 'none',
          varsIgnorePattern: 'React',
          ignoreRestSiblings: true,
        },
      ],
      '@typescript-eslint/no-restricted-imports': [
        'error',
        {
          paths: [
            {
              name: 'lodash',
              message:
                'Please use sub imports (eg, lodash/get) instead of the global lodash import.',
              allowTypeImports: true,
            },
          ],
        },
      ],
      '@typescript-eslint/no-empty-object-type': 'warn',
      '@typescript-eslint/no-unsafe-function-type': 'warn',
      '@typescript-eslint/no-wrapper-object-types': 'warn',
      '@typescript-eslint/no-empty-function': 'warn',
      '@typescript-eslint/no-namespace': 'warn',
      '@typescript-eslint/no-empty-interface': 'warn',
      '@typescript-eslint/no-var-requires': 'warn',
      '@typescript-eslint/no-this-alias': 'warn',
      '@typescript-eslint/no-explicit-any': 'off',
    },
  },
  ...nx.configs['flat/javascript'],
  {
    files: ['**/*.ts', '**/*.tsx', '**/*.js', '**/*.jsx'],
    rules: {
      'no-case-declarations': 'warn',
      'no-unsafe-optional-chaining': 'warn',
      'no-useless-catch': 'warn',
      'no-case-declarations': 'warn',
      'no-restricted-globals': 'warn',
      'no-prototype-builtins': 'warn',
      'no-unsafe-optional-chaining': 'warn',
      'no-useless-catch': 'warn',
      'no-useless-assignment': 'warn',
      'preserve-caught-error': 'warn',
      '@typescript-eslint/no-unused-expressions': 'off',
    },
  },
  {
    files: ['**/*.ts', '**/*.tsx', '**/*.js', '**/*.jsx'],
    rules: {
      'import/no-unresolved': 'off',
      'import/named': 'off',
      'import/no-named-as-default': 'off',
      'no-restricted-imports': [
        'error',
        {
          paths: [
            {
              name: 'lodash',
              message:
                'Please use sub imports (eg, lodash/get) instead of the global lodash import.',
            },
          ],
        },
      ],
    },
  },
  {
    files: ['**/*.js', '**/*.jsx'],
    rules: {
      'no-unused-vars': [
        'warn',
        {
          args: 'none',
          varsIgnorePattern: 'React',
        },
      ],
      '@typescript-eslint/no-useless-constructor': 'off',
    },
  },
  {
    files: ['**/*.stories.tsx'],
    rules: {
      'react-hooks/rules-of-hooks': 'off',
      'react/jsx-key': 'off',
      'react-hooks/refs': 'off',
    },
  },
  {
    // TypeScript only: TS type-checks component props, so eslint-plugin-react's
    // runtime `prop-types` rule is redundant there (its own docs recommend
    // disabling it under TS). Scoped to .ts/.tsx so legacy .js/.jsx components
    // KEEP `react/prop-types` active.
    files: ['**/*.ts', '**/*.tsx'],
    rules: {
      'react/prop-types': 'off',
    },
  },
  {
    ignores: ['**/vitest.config.*.timestamp*', '**/vite.config.*.timestamp*'],
  },
];
