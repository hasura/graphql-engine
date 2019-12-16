const { getActionDefinitionFromSdl, getServerTypesFromSdl  } = require('../../../shared/utils/sdlUtils');

const handlePayload = (payload) => {

  const response = {
    body: null,
    status: 200
  };

  const { sdl, types: existingTypes } = payload;

  const { types: customTypes, error: typesParseError } = getServerTypesFromSdl(sdl.types, existingTypes);

  if (typesParseError) {
    response.body = {
      error: typesParseError
    };
    response.status = 400;
    return response;
  }

  const { name: actionName, arguments: args, outputType, error: actionsParseError } = getActionDefinitionFromSdl(sdl.action);

  if (actionsParseError) {
    response.body = {
      error: actionsParseError
    };
    response.status = 400;
  }

  response.body = {
    action: {
      arguments: args,
      output_type: outputType,
      name: actionName
    },
    types: customTypes
  };

  return response;
}

const requestHandler = (payload) => {

  const {
    body, status
  } = handlePayload(payload)

  return JSON.stringify(body);

};

module.exports = requestHandler;
module.exports.handlePayload = handlePayload;









