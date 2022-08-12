/* eslint-disable */
import globals from '../../../../Globals';
import {
  CODEGEN_REPO,
  ALL_FRAMEWORKS_FILE_PATH,
  BASE_CODEGEN_PATH,
} from '../constants';
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
import { getPersistedDerivedAction } from '../utils';
import requestAction from '../../../../utils/requestAction';
import requestActionPlain from '../../../../utils/requestActionPlain';

export const getCodegenFilePath = framework => {
  return `${BASE_CODEGEN_PATH}/${framework}/actions-codegen.js`;
};

export const getStarterKitPath = framework => {
  return `https://github.com/${CODEGEN_REPO}/tree/master/${framework}/starter-kit/`;
};

export const getStarterKitDownloadPath = framework => {
  return `https://github.com/${CODEGEN_REPO}/raw/master/${framework}/${framework}.zip`;
};
export const getGlitchProjectURL = () => {
  return 'https://glitch.com/edit/?utm_content=project_hasura-actions-starter-kit&utm_source=remix_this&utm_medium=button&utm_campaign=glitchButton#!/remix/hasura-actions-starter-kit';
};

export const GLITCH_PROJECT_URL = '';

export const getAllCodegenFrameworks = dispatch => {
  return dispatch(requestAction(ALL_FRAMEWORKS_FILE_PATH, {}));
};

export const getCodegenFunc = (framework, dispatch) => {
  return dispatch(requestActionPlain(getCodegenFilePath(framework)))
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
  parentOperation,
  dispatch
) => {
  return getCodegenFunc(framework, dispatch)
    .then(codegenerator => {
      const derive = {
        operation: parentOperation,
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
