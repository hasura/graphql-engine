const { getTypesSdl, getActionDefinitionSdl } = require('../../../shared/utils/sdlUtils');
const { deriveMutation } = require('../../derive/derive')

const handlePayload = (payload) => {
  const { action, types, derive } = payload;
  const {
    mutation: toDeriveMutation,
    introspection_schema: introspectionSchema
  } = derive || {};

  const {
    name: deriveMutationName,
    action_name: actionName
  } = toDeriveMutation || {};

  const response = {
    body: null,
    status: 200
  };

  let actionSdl;
  let typesSdl;
  let actionSdlError, typesSdlError, deriveMutationError;

  if (action) {
    try {
      actionSdl = getActionDefinitionSdl(action);
    } catch (e) {
      actionsError = e;
    }
  }

  if (types && !deriveMutationName) {
    try {
      typesSdl = getTypesSdl(types);
    } catch (e) {
      typesSdlError = e;
    }
  }

  if (deriveMutationName) {
    try {
      const derivation = deriveMutation(deriveMutationName, introspectionSchema, types, actionName);
      actionSdl = getActionDefinitionSdl(derivation.action);
      const derivedTypesSdl = getTypesSdl(derivation.types);
      const existingTypesSdl = getTypesSdl(types);
      typesSdl = `${derivedTypesSdl}\n\n${existingTypesSdl}`;
    } catch (e) {
      deriveMutationError = e;
    }
  }

  if (actionSdlError) {
    response.body = {
      error: 'invalid action definition'
    };
    response.status = 400;
    return response;
  }

  if (deriveMutationError) {
    response.body = {
      error: `could not derive mutation: ${deriveMutationError.message}`
    };
    response.status = 400;
    return response;
  }

  if (typesSdlError) {
    response.body = {
      error: 'invalid types'
    };
    response.status = 400;
    return response;
  }

  response.body = {
    sdl: {
      action: actionSdl || null,
      types: typesSdl || null
    }
  };

  return response;
}

const requestHandler = (payload) => {


  const {
    body, status
  } = handlePayload(payload)

  return JSON.stringify(body);

}

module.exports = requestHandler;
module.exports.handlePayload = handlePayload;
