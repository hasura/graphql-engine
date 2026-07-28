// @ts-check

import js from '@eslint/js';
import { defineConfig } from 'eslint/config';
import tseslint from 'typescript-eslint';

export default defineConfig({
  files: ['**/*.{js,ts}'],
  extends: [js.configs.recommended, tseslint.configs.recommended],
  rules: {
    "no-undef": "off",
    "no-fallthrough": "off",
    "preserve-caught-error": "off",
    "no-useless-catch": "off",
    "no-case-declarations": "off",
    "no-unassigned-vars": "off",
    "no-useless-assignment": "off",
    "@typescript-eslint/no-unused-vars": "off",
    "@typescript-eslint/no-explicit-any": "off",
    "@typescript-eslint/no-require-imports": "off",
  }
});