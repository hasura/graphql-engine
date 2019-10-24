import defaultState from './state';

const SET_DEFAULTS = 'Actions/Add/SET_DEFAULTS';
export const setDefaults = () => ({ type: SET_DEFAULTS });

const SET_ACTION_NAME = 'Actions/Add/SET_ACTION_NAME';
export const setActionName = name => ({
  type: SET_ACTION_NAME,
  name,
});

const SET_ACTION_WEBHOOK = 'Actions/Add/SET_ACTION_WEBHOOK';
export const setActionWebhook = webhook => ({
  type: SET_ACTION_WEBHOOK,
  webhook,
});

const SET_ACTION_ARGUMENTS = 'Actions/Add/SET_ACTION_ARGUMENTS';
export const setActionArguments = args => ({
  type: SET_ACTION_ARGUMENTS,
  args,
});

const SET_ACTION_OUTPUT_TYPE = 'Actions/Add/SET_ACTION_OUTPUT_TYPE';
export const setActionOutputType = outputType => ({
  type: SET_ACTION_OUTPUT_TYPE,
  outputType,
});

const SET_TYPES = 'Actions/Add/SET_TYPES';
export const setTypes = types => ({
  type: SET_TYPES,
  types,
});

// used to set types, args and output types together
const SET_TYPES_BULK = 'Actions/Add/SET_TYPES_BULK';
export const setTypesBulk = (types, args, outputType) => ({
  type: SET_TYPES_BULK,
  args,
  types,
  outputType,
});

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_DEFAULTS:
      return defaultState;
    case SET_ACTION_NAME:
      return {
        ...state,
        name: action.name,
      };
    case SET_ACTION_WEBHOOK:
      return {
        ...state,
        webhook: action.webhook,
      };
    case SET_ACTION_ARGUMENTS:
      return {
        ...state,
        arguments: action.args,
      };
    case SET_ACTION_OUTPUT_TYPE:
      return {
        ...state,
        outputType: action.outputType,
      };
    case SET_TYPES:
      return {
        ...state,
        types: action.types,
      };
    case SET_TYPES_BULK:
      return {
        ...state,
        types: action.types,
        arguments: action.args,
        outputType: action.outputType,
      };
    default:
      return state;
  }
};

export default reducer;
