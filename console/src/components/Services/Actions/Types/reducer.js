import defaultState from './state';

const SET_MODIFY_STATE = 'Actions/Modify/SET_MODIFY_STATE';
export const setModifyState = state => ({
  type: SET_MODIFY_STATE,
  state,
});

const SET_ACTION_WEBHOOK = 'Actions/Modify/SET_ACTION_WEBHOOK';
export const setActionWebhook = webhook => ({
  type: SET_ACTION_WEBHOOK,
  webhook,
});

const SET_ACTION_KIND = 'Actions/Modify/SET_ACTION_KIND';
export const setActionKind = kind => ({
  type: SET_ACTION_KIND,
  kind,
});

const SET_ACTION_DEFINITION = 'Actions/Add/SET_ACTION_DEFINITION';
export const setActionDefinition = (sdl, error) => ({
  type: SET_ACTION_DEFINITION,
  definition: { sdl, error },
});

const SET_TYPE_DEFINITION = 'Actions/Add/SET_TYPE_DEFINITION';
export const setTypeDefinition = (sdl, error = null) => ({
  type: SET_TYPE_DEFINITION,
  definition: { sdl, error },
});

const SET_FETCHING = 'Actions/Modify/SET_FETCHING';
export const setFetching = () => ({ type: SET_FETCHING });
const UNSET_FETCHING = 'Actions/Modify/UNSET_FETCHING';
export const unsetFetching = () => ({ type: UNSET_FETCHING });

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_MODIFY_STATE:
      return {
        ...action.state,
      };
    case SET_ACTION_WEBHOOK:
      return {
        ...state,
        webhook: action.webhook,
      };
    case SET_ACTION_KIND:
      return {
        ...state,
        kind: action.kind,
      };
    case SET_FETCHING:
      return {
        ...state,
        isFetching: true,
      };
    case UNSET_FETCHING:
      return {
        ...state,
        isFetching: false,
      };
    case SET_ACTION_DEFINITION:
      return {
        ...state,
        actionDefinition: action.definition,
      };
    case SET_TYPE_DEFINITION:
      return {
        ...state,
        typeDefinition: action.definition,
      };
    default:
      return state;
  }
};

export default reducer;
