const { buildSchema, printSchema, parse } = require('graphql');
const { codegen } = require('@graphql-codegen/core');
const typescriptPlugin = require('@graphql-codegen/typescript');
const { camelize } = require('inflection');

const fetch = require('node-fetch');
const path = require('path')
const fs = require('fs');
const { getTemplatePath } = require('../../utils/utils')

const CODEGENERATOR_NOT_FOUND = 'given codegen framework not found';
const FILE_SYSTEM_PATH = 'fs_path';
const URL_PATH = 'url path';

const resolveCodegeneratorPath = (codegenConfig) => {
  let { framework } = codegenConfig;
  let codegeneratorPath = codegenConfig.uri;
  if (!codegeneratorPath) {
    codegeneratorPath = getTemplatePath(framework)
  }
  return codegeneratorPath;
};

const resolveCodegeneratorFromUrl = async (url) => {
  let codegenerator;
  try {
    const fetchResp = await fetch(url);
    if (fetchResp.status >= 300) {
      throw Error(CODEGENERATOR_NOT_FOUND);
    }
    const codegeneratorText = await fetchResp.text()
    eval(`${codegeneratorText} codegenerator = templater`);
    return codegenerator;
  } catch (e) {
    throw e;
  }
};

const resolveCodegeneratorFromFs = async (fsPath) => {
  let codegenerator;
  try {
    const codegeneratorText = fs.readFileSync(path.resolve(fsPath), { encoding: 'utf8'});
    eval(`${codegeneratorText}\n codegenerator = templater`);
    return codegenerator;
  } catch (e) {
    throw e;
  }
};

const resolveCodegenerator = async (codegenConfig) => {

  const codegeneratorPath = resolveCodegeneratorPath(codegenConfig);
  if (!codegeneratorPath) {
    throw Error(CODEGENERATOR_NOT_FOUND)
  }

  let codegenerator
  let pathType = URL_PATH;
  try {
    new URL(codegeneratorPath)
  } catch (_) {
    pathType = FILE_SYSTEM_PATH;
  }

  try {
    if (pathType === FILE_SYSTEM_PATH) {
      codegenerator = await resolveCodegeneratorFromFs(codegeneratorPath)
    } else {
      codegenerator = await resolveCodegeneratorFromUrl(codegeneratorPath);
    }
  } catch (e) {
    throw e;
  }

  return codegenerator;

}

const getCodegenFiles = async (actionName, actionsSdl, derive, codegenConfig) => {

  let codegenerator;
  try {
    codegenerator = await resolveCodegenerator(codegenConfig)
  } catch (e) {
    throw e;
  }

  let codegenFiles = codegenerator(actionName, actionsSdl, derive);
  if (codegenFiles && codegenFiles.constructor.name === 'Promise') {
    codegenFiles = await codegenFiles;
  }

  return codegenFiles;

}

const getFrameworkCodegen = async (actionName, actionsSdl, derive, codegenConfig) => {

  try {
    const codegenFiles = await getCodegenFiles(actionName, actionsSdl, derive, codegenConfig);
    return {
      files: codegenFiles
    }
  } catch (e) {
    return {
      error: e.message
    }
  }

};

module.exports = {
  getFrameworkCodegen
};
