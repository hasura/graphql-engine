const {
  getTypesSdl,
  getActionDefinitionSdl,
} = require('../../../shared/utils/sdlUtils');
const deriveAction = require('../../../shared/utils/deriveAction').default;

const handlePayload = (payload) => {
  const { actions, types, derive } = payload;
  const {
    operation: toDeriveOperation,
    introspection_schema: introspectionSchema,
    action_name: actionName,
  } = derive || {};

  const response = {
    body: null,
    status: 200,
  };

  let actionSdl = '';
  let typesSdl = '';
  let actionSdlError;
  let typesSdlError;
  let deriveActionError;

  if (actions) {
    try {
      actions.forEach((a) => {
        actionSdl += `${getActionDefinitionSdl(
          a.name,
          a.definition.type,
          a.definition.arguments,
          a.definition.output_type,
        )}\n`;
      });
    } catch (e) {
      actionSdlError = e;
    }
  }

  if (types) {
    try {
      typesSdl = getTypesSdl(types);
    } catch (e) {
      typesSdlError = e;
    }
  }

  let sdl = `${actionSdl}\n\n${typesSdl}`;

  if (toDeriveOperation) {
    try {
      const derivation = deriveAction(
        toDeriveOperation,
        introspectionSchema,
        actionName,
      );
      const derivedActionSdl = getActionDefinitionSdl(
        derivation.action.name,
        derivation.action.type,
        derivation.action.arguments,
        derivation.action.output_type,
      );
      const derivedTypesSdl = getTypesSdl(derivation.types);
      sdl = `${derivedActionSdl}\n\n${derivedTypesSdl}\n\n${sdl}`;
    } catch (e) {
      deriveActionError = e;
    }
  }

  if (actionSdlError) {
    response.body = {
      error: 'invalid actions definition',
    };
    response.status = 400;
    return response;
  }

  if (deriveActionError) {
    response.body = {
      error: `could not derive action: ${deriveActionError.message}`,
    };
    response.status = 400;
    return response;
  }

  if (typesSdlError) {
    response.body = {
      error: 'invalid types',
    };
    response.status = 400;
    return response;
  }

  response.body = {
    sdl: {
      complete: sdl,
    },
  };

  return response;
};

const requestHandler = (payload) => {
  const { body, status } = handlePayload(payload);

  return body;
};

module.exports = requestHandler;
module.exports.handlePayload = handlePayload;
