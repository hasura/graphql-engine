import defaultState from './State';

const UPDATE_LOG_SETTINGS = 'Metrics/UPDATE_LOG_SETTINGS';

const metricsReducer = (state = defaultState, action) => {
  switch (action.type) {
    case UPDATE_LOG_SETTINGS:
      return { ...state, projectConfig: action.data };
    default:
      return state;
  }
};

export default metricsReducer;
export { UPDATE_LOG_SETTINGS };
