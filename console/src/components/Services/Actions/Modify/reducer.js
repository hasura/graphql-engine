import defaultState from './state';

const SET_MODIFY_STATE = 'Actions/Modify/SET_MODIFY_STATE';
export const setModifyState = state => ({
  type: SET_MODIFY_STATE,
  state,
});

const SET_ACTION_NAME = 'Actions/Modify/SET_ACTION_NAME';
export const setActionName = name => ({
  type: SET_ACTION_NAME,
  name,
});

const SET_ACTION_WEBHOOK = 'Actions/Modify/SET_ACTION_WEBHOOK';
export const setActionWebhook = webhook => ({
  type: SET_ACTION_WEBHOOK,
  webhook,
});

const SET_ACTION_ARGUMENTS = 'Actions/Modify/SET_ACTION_ARGUMENTS';
export const setActionArguments = args => ({
  type: SET_ACTION_ARGUMENTS,
  args,
});

const SET_ACTION_OUTPUT_TYPE = 'Actions/Modify/SET_ACTION_OUTPUT_TYPE';
export const setActionOutputType = outputType => ({
  type: SET_ACTION_OUTPUT_TYPE,
  outputType,
});

const SET_TYPES = 'Actions/Modify/SET_TYPES';
export const setTypes = types => ({
  type: SET_TYPES,
  types,
});

// used to set types, args and output types together
const SET_TYPES_BULK = 'Actions/Modify/SET_TYPES_BULK';
export const setTypesBulk = (types, args, outputType) => ({
  type: SET_TYPES_BULK,
  args,
  types,
  outputType,
});

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_MODIFY_STATE:
      return {
        ...action.state,
      };
    default:
      return state;
  }
};

export default reducer;
