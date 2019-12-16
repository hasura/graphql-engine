const fetch = require('node-fetch');
const { getTemplatePath } = require('../../utils/utils')
const { parseCustomTypes, getActionTypes } = require('../../utils/typeUtils')
const { getFrameworkScaffold } = require('./template');
const { getActionDefinitionSdl, getTypesSdl } = require('../sdl/utils')
const { parse: sdlParse } = require('graphql/language/parser');

const getActionScaffold = async (payload) => {

  const {
    framework,
    action: {
      action_name: actionName,
      action_defn: actionDef
    },
    types: serverTypes
  } = payload;

  const mutationSdl = getActionDefinitionSdl({
    ...actionDef,
    name: actionName
  });

  const typesSdl = getTypesSdl(parseCustomTypes(serverTypes));

  try {
    const scaffoldResp = await getFrameworkScaffold(framework, mutationSdl, typesSdl)
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

