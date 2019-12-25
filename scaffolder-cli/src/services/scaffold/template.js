const fetch = require('node-fetch');
const path = require('path')
const fs = require('fs');
const templaters = require('../../templaters');
const { getTemplatePath } = require('../../utils/utils')

const SCAFFOLDER_NOT_FOUND = 'given scaffolder not found';
const FILE_SYSTEM_PATH = 'fs_path';
const URL_PATH = 'url path';
const ERROR_IGNORE = 'error ignore'

const resolveScaffolderPath = (framework, scaffoldConfig) => {
  let scaffolderName = framework || scaffoldConfig.default;
  if (!scaffolderName) return;
  let scaffolderPath = scaffoldConfig.custom_scaffolders && scaffoldConfig.custom_scaffolders[scaffolderName];
  if (!scaffolderPath) {
    scaffolderPath = getTemplatePath(scaffolderName)
  }
  return scaffolderPath;
}

const resolveScaffolderFromUrl = async (url) => {
  let scaffolder;
  try {
    const fetchResp = await fetch(url);
    if (fetchResp.status >= 300) {
      throw Error(SCAFFOLDER_NOT_FOUND);
    }
    const scaffolderText = await fetchResp.text()
    eval(`${scaffolderText} scaffolder = templater`);
    return scaffolder;
  } catch (e) {
    throw e;
  }
}

const resolveScaffolderFromFs = async (fsPath) => {
  let scaffolder;
  try {
    const scaffolderText = fs.readFileSync(path.resolve(fsPath), { encoding: 'utf8'});
    eval(`${scaffolderText} scaffolder = templater`);
    return scaffolder;
  } catch (e) {
    throw e;
  }
}

const resolveScaffolder = async (framework, scaffoldConfig) => {
  const scaffolderPath = resolveScaffolderPath(framework, scaffoldConfig);
  let scaffolder
  if (!scaffolderPath) {
    throw Error(ERROR_IGNORE);
  }

  let pathType = URL_PATH;
  try {
    new URL(scaffolderPath)
  } catch (_) {
    pathType = FILE_SYSTEM_PATH;
  }

  try {
    if (pathType === FILE_SYSTEM_PATH) {
      scaffolder = await resolveScaffolderFromFs(scaffolderPath)
    } else {
      scaffolder = await resolveScaffolderFromUrl(scaffolderPath);
    }
  } catch (e) {
    if (!framework) {
      throw Error(ERROR_IGNORE);
    } else {
      throw e
    }
  }
  
  return scaffolder;

}

const getScaffoldFiles = async (framework, actionName, actionsSdl, derive, scaffoldConfig) => {
  let scaffolder;
  try {
    scaffolder = await resolveScaffolder(framework, scaffoldConfig)
  } catch (e) {
    if (e.message === ERROR_IGNORE) {
      return [];
    } else {
      throw e;
    }
  }

  let scaffolds = scaffolder(actionName, actionsSdl, derive);
  if (scaffolds && scaffolds.constructor.name === 'Promise') {
    scaffolds = await scaffolds;
  }

  return scaffolds;

}

const getFrameworkScaffold = async (framework, actionName, actionsSdl, derive, scaffoldConfig) => {

  try {
    const scaffoldFiles = await getScaffoldFiles(framework, actionName, actionsSdl, derive, scaffoldConfig);
    return {
      files: scaffoldFiles
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
  getFrameworkScaffold 
};
