const fetch = require('node-fetch');
const { getTemplatePath } = require('../../utils/utils')
const { parseCustomTypes, getActionTypes } = require('../../shared/utils/hasuraCustomTypeUtils')
const { getFrameworkScaffold } = require('./template');
const { getActionDefinitionSdl, getTypesSdl } = require('../../shared/utils/sdlUtils');
const { parse: sdlParse } = require('graphql/language/parser');

const getActionScaffold = async (payload) => {

  const {
    framework,
    action_name: actionName,
    sdl: {
      complete: sdlComplete
    }
  } = payload;

  try {
    const scaffoldResp = await getFrameworkScaffold(framework, actionName, sdlComplete)
    if (scaffoldResp.error) {
      throw Error(scaffoldResp.error)
    } else {
      return scaffoldResp.files
    }
  } catch (e) {
    throw e;
  }
};

module.exports = {
  getActionScaffold
}

