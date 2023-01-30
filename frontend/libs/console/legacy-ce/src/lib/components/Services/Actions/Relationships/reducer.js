import defaultState from './state';

const SET_TYPES = 'Actions/Rel/SET_INITIAL_TYPES';
export const setTypes = types => ({ type: SET_TYPES, types });

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_TYPES:
      return {
        ...state,
        types: action.types,
      };
    default:
      return state;
  }
};

export default reducer;
