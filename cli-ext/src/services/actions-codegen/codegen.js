const fetch = require('node-fetch');
const { parse: sdlParse } = require('graphql/language/parser');
const { getTemplatePath } = require('../../utils/utils');
const {
  parseCustomTypes,
  getActionTypes,
} = require('../../shared/utils/hasuraCustomTypeUtils');
const { getFrameworkCodegen } = require('./template');
const {
  getActionDefinitionSdl,
  getTypesSdl,
} = require('../../shared/utils/sdlUtils');

const getActionsCodegen = async (payload) => {
  const {
    action_name: actionName,
    sdl: { complete: sdlComplete },
    derive,
    codegen_config: codegenConfig,
  } = payload;

  try {
    const codegenResp = await getFrameworkCodegen(
      actionName,
      sdlComplete,
      derive,
      codegenConfig,
    );
    if (codegenResp.error) {
      throw Error(codegenResp.error);
    } else {
      return codegenResp.files;
    }
  } catch (e) {
    throw e;
  }
};

module.exports = {
  getActionsCodegen,
};
