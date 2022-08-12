import defaultState from './state';

export const LOADING_TYPES = 'Types/LOADING_TYPES';
export const LOADING_TYPES_FAILURE = 'Types/LOADING_TYPES_FAILURE';

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case LOADING_TYPES:
      return {
        ...state,
        isFetching: true,
      };
    case LOADING_TYPES_FAILURE:
      return {
        ...state,
        isFetching: false,
        error: action.error,
      };

    default:
      return state;
  }
};

export default reducer;
