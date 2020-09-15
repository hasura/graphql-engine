const {
  getAllActionsFromSdl,
  getAllTypesFromSdl,
} = require('../../../shared/utils/sdlUtils');
const {
  reformCustomTypes,
} = require('../../../shared/utils/hasuraCustomTypeUtils');

const handlePayload = (payload) => {
  const response = {
    body: null,
    status: 200,
  };

  const { sdl } = payload;

  let customTypes;
  let typesParseError;

  if (!sdl.complete.trim()) {
    response.body = {
      actions: [],
      types: reformCustomTypes([]),
    };
    return response;
  }

  try {
    customTypes = getAllTypesFromSdl(sdl.complete);
  } catch (e) {
    typesParseError = e;
  }

  if (typesParseError) {
    response.body = {
      error: typesParseError.message,
    };
    response.status = 400;
    return response;
  }

  let allActions;
  let actionsParseError;
  try {
    allActions = getAllActionsFromSdl(sdl.complete);
  } catch (e) {
    actionsParseError = e;
  }

  if (actionsParseError) {
    response.body = {
      error: actionsParseError.message,
    };
    response.status = 400;
  }

  response.body = {
    actions: allActions,
    types: customTypes,
  };

  return response;
};

const requestHandler = (payload) => {
  const { body, status } = handlePayload(payload);

  return body;
};

module.exports = requestHandler;
module.exports.handlePayload = handlePayload;
