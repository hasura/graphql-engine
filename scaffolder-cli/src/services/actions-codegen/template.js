const fetch = require('node-fetch');
const path = require('path')
const fs = require('fs');
const templaters = require('../../templaters');
const { getTemplatePath } = require('../../utils/utils')

const CODEGENERATOR_NOT_FOUND = 'given codegen framework not found';
const FILE_SYSTEM_PATH = 'fs_path';
const URL_PATH = 'url path';
const ERROR_IGNORE = 'error ignore'

const resolveCodegeneratorPath = (framework, codegenConfig) => {
  let codegeneratorName = framework || codegenConfig.default;
  if (!codegeneratorName) return;
  let codegeneratorPath = codegenConfig.custom_codegenerators ? codegenConfig.custom_codegenerators[codegeneratorName] : null;
  if (!codegeneratorPath) {
    codegeneratorPath = getTemplatePath(codegeneratorName)
  }
  return codegeneratorPath;
};

const resolveCodegeneratorFromUrl = async (url) => {
  let codegenerator;
  try {
    const fetchResp = await fetch(url);
    if (fetchResp.status >= 300) {
      throw Error(_NOT_FOUND);
    }
    const codegneratorText = await fetchResp.text()
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
    eval(`${codegeneratorText} scaffolder = templater`);
    return codegenerator;
  } catch (e) {
    throw e;
  }
};

const resolveCodegenerator = async (framework, codegenConfig) => {
  const codegeneratorPath = resolveCodegeneratorPath(framework, codegenConfig);
  let codegenerator
  if (!codegenerator) {
    throw Error(ERROR_IGNORE);
  }

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
    if (!framework) {
      throw Error(ERROR_IGNORE);
    } else {
      throw e
    }
  }
  
  return codegenerator;

}

const getCodegenFiles = async (framework, actionName, actionsSdl, derive, codegenConfig) => {
  let codegenerator;
  try {
    codegenerator = await resolveCodegenerator(framework, codegenConfig)
  } catch (e) {
    if (e.message === ERROR_IGNORE) {
      return [];
    } else {
      throw e;
    }
  }

  let codegenFiles = codegenerator(actionName, actionsSdl, derive);
  if (codegenFiles && codegenFiles.constructor.name === 'Promise') {
    codegenFiles = await codegenFiles;
  }

  return codegenFiles;

}

const getFrameworkCodegen = async (framework, actionName, actionsSdl, derive, scaffoldConfig) => {

  try {
    const codegenFiles = await getCodegenFiles(framework, actionName, actionsSdl, derive, scaffoldConfig);
    return {
      files: codegenFiles
    }
  } catch (e) {
    return {
      error: e.message
    }
  }

};
/*
let aSdl = `
type Mutation { actionName1 (arg1: SampleInput!): SampleOutput }
type SampleOutput { accessToken: String! }
input SampleInput { username: String! password: String! }
type Mutation { actionName2 (arg1: SampleInput!): SampleOutput }
`

let aName = 'actionName2';

getFrameworkScaffold('nodejs-zeit', aName, aSdl).then(s => console.log(s)).catch(e => console.log(e));
*/
module.exports = {
  getFrameworkCodegen
};
