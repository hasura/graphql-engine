import defaultState from './state';
import { SET_CURRENT_TRIGGER, RAEvents, RASetCurrentTrigger } from './types';

export const setCurrentTrigger = (
  triggerName: string
): RASetCurrentTrigger => ({
  type: SET_CURRENT_TRIGGER,
  name: triggerName,
});

const reducer = (state = defaultState, action: RAEvents) => {
  switch (action.type) {
    case SET_CURRENT_TRIGGER:
      return {
        ...state,
        currentTrigger: action.name,
      };
    default:
      return state;
  }
};

export default reducer;
