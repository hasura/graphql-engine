const { buildSchema, printSchema, parse } = require('graphql');
const { codegen } = require('@graphql-codegen/core');
const typescriptPlugin = require('@graphql-codegen/typescript');
const { camelize } = require('inflection');

export const importDeps = () => {
  console.log(buildSchema);
};