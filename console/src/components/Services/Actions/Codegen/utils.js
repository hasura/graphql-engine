/* eslint-disable */
import { CODEGEN_REPO, ALL_FRAMEWORKS_FILE_PATH } from '../constants';

const {
  buildClientSchema,
  validate,
  printSchema,
  parse,
  isNonNullType,
  isListType,
  isWrappingType,
  isScalarType,
} = require('graphql');
const { camelize } = require('inflection');

export const getCodegenFilePath = framework => {
  return `https://raw.githubusercontent.com/${CODEGEN_REPO}/master/${framework}/codegen.js`;
};

export const getAllCodegenFrameworks = () => {
  return fetch(ALL_FRAMEWORKS_FILE_PATH)
    .then(r => r.json())
    .catch(e => {
      console.error('could not fetch the latest codegen file');
      throw e;
    });
};

export const getCodegenFunc = framework => {
  process.hrtime = () => null;
  return fetch(getCodegenFilePath(framework))
    .then(r => r.text())
    .then(rawJsString => {
      return import('@graphql-codegen/core').then(({ codegen }) => {
        return import('@graphql-codegen/typescript').then(typescriptPlugin => {
          let codegenerator;
          eval(`${rawJsString}\ncodegenerator = templater;`);
          return codegenerator;
        });
      });
    })
    .catch(e => {
      throw e;
    });
};

export const getFrameworkCodegen = (framework, actionName, actionsSdl) => {
  return getCodegenFunc(framework)
    .then(codegenerator => {
      const codegenFiles = codegenerator(actionName, actionsSdl);
      return codegenFiles;
    })
    .catch(e => {
      console.error('unexpected error', e);
      throw e;
    });
};
