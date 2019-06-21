/* Default state */
const defaultState = {
  headers: [
    {
      name: '',
      type: '',
      value: '',
    },
  ],
};

/* */

/* Action constants */
const generateHeaderSyms = (prefix = 'API_HEADER') => {
  // TODO: change this anti-pattern
  // There is no way to guarantee if the derived constants actually exists. The whole point of using constants is lost
  return {
    HEADER_KEY_CHANGE: `${prefix}/HEADER_KEY_CHANGE`,
    HEADER_VALUE_TYPE_CHANGE: `${prefix}/HEADER_VALUE_TYPE_CHANGE`,
    HEADER_VALUE_CHANGE: `${prefix}/HEADER_VALUE_CHANGE`,
    UPDATE_HEADERS: `${prefix}/UPDATE_HEADERS`,
    RESET_HEADER: `${prefix}/RESET_HEADER`,
    ADD_NEW_HEADER: `${prefix}/ADD_NEW_HEADER`,
    DELETE_HEADER: `${prefix}/DELETE_HEADER`,
  };
};
/* */

const generateReducer = (eventPrefix, defaultHeaders) => {
  /* Action constants */
  if (defaultHeaders && defaultHeaders.length > 0) {
    defaultState.headers = [...defaultHeaders];
  }
  const {
    HEADER_KEY_CHANGE,
    HEADER_VALUE_CHANGE,
    HEADER_VALUE_TYPE_CHANGE,
    RESET_HEADER,
    DELETE_HEADER,
    ADD_NEW_HEADER,
    UPDATE_HEADERS,
  } = generateHeaderSyms(eventPrefix);
  /* */

  /* Reducer */
  const headerReducer = (state = defaultState, action) => {
    switch (action.type) {
      case HEADER_KEY_CHANGE:
        return {
          ...state,
          headers: [
            ...state.headers.slice(0, action.data.index),
            {
              ...state.headers[action.data.index],
              name: action.data.name,
            },
            ...state.headers.slice(action.data.index + 1, state.headers.length),
          ],
        };
      case HEADER_VALUE_TYPE_CHANGE:
        return {
          ...state,
          headers: [
            ...state.headers.slice(0, action.data.index),
            {
              ...state.headers[action.data.index],
              type: action.data.type,
            },
            ...state.headers.slice(action.data.index + 1, state.headers.length),
          ],
        };
      case HEADER_VALUE_CHANGE:
        return {
          ...state,
          headers: [
            ...state.headers.slice(0, action.data.index),
            {
              ...state.headers[action.data.index],
              value: action.data.value,
            },
            ...state.headers.slice(action.data.index + 1, state.headers.length),
          ],
        };
      case ADD_NEW_HEADER:
        return {
          ...state,
          headers: [...state.headers, { ...defaultState.headers[0] }],
        };

      case DELETE_HEADER:
        return {
          ...state,
          headers: [
            ...state.headers.slice(0, action.data.index),
            ...state.headers.slice(action.data.index + 1, state.headers.length),
          ],
        };
      case RESET_HEADER:
        return {
          ...defaultState,
        };
      case UPDATE_HEADERS:
        return {
          ...state,
          headers: [...action.data],
        };
      default:
        return {
          ...state,
        };
    }
  };
  return headerReducer;
  /* */
};

export { generateHeaderSyms };
export default generateReducer;
