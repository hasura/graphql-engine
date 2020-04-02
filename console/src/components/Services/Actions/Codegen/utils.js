/* eslint-disable */
import globals from '../../../../Globals';
import { CODEGEN_REPO, ALL_FRAMEWORKS_FILE_PATH } from '../constants';
import endpoints from '../../../../Endpoints';

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
import { getPersistedDerivedAction } from '../lsUtils';

export const getCodegenFilePath = framework => {
  return `${globals.assetsPath}/common/codegen/${framework}/actions-codegen.js`;
};

export const getStarterKitPath = framework => {
  return `https://github.com/${CODEGEN_REPO}/tree/master/${framework}/starter-kit/`;
};

export const getGlitchProjectURL = () => {
  return 'https://glitch.com/edit/?utm_content=project_hasura-actions-starter-kit&utm_source=remix_this&utm_medium=button&utm_campaign=glitchButton#!/remix/hasura-actions-starter-kit';
};

export const GLITCH_PROJECT_URL = '';

export const getAllCodegenFrameworks = () => {
  return fetch(ALL_FRAMEWORKS_FILE_PATH)
    .then(r => r.json())
    .catch(e => {
      console.error('could not fetch the latest codegen file');
      throw e;
    });
};

export const getCodegenFunc = framework => {
  return fetch(getCodegenFilePath(framework))
    .then(r => r.text())
    .then(rawJsString => {
      const { codegen } = require('@graphql-codegen/core');
      const typescriptPlugin = require('@graphql-codegen/typescript');
      let codegenerator;
      eval(`${rawJsString}\ncodegenerator = templater;`);
      return codegenerator;
    })
    .catch(e => {
      throw e;
    });
};

export const getFrameworkCodegen = (
  framework,
  actionName,
  actionsSdl,
  parentMutation
) => {
  return getCodegenFunc(framework)
    .then(codegenerator => {
      const derive = {
        operation: parentMutation,
        endpoint: endpoints.graphQLUrl,
      };
      const codegenFiles = codegenerator(actionName, actionsSdl, derive);
      return codegenFiles;
    })
    .catch(e => {
      console.error('unexpected error', e);
      throw e;
    });
};
