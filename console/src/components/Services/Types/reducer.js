import defaultState from './state';

export const LOADING_TYPES = 'Types/LOADING_TYPES';
export const LOADING_TYPES_SUCCESS = 'Types/LOADING_TYPES_SUCCESS';
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
    case LOADING_TYPES_SUCCESS:
      if (action.types) {
        return {
          ...state,
          isFetching: false,
          types: action.types,
        };
      }
      return state;

    default:
      return state;
  }
};

export default reducer;
