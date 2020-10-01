import defaultState from './state';
import {
  ScheduledTrigger,
  LOADED_SCHEDULED_TRIGGERS,
  SET_CURRENT_TRIGGER,
  RAEvents,
  RASetCurrentTrigger,
  RASetScheduledTriggers,
} from './types';

export const setScheduledTriggers = (
  data: ScheduledTrigger[]
): RASetScheduledTriggers => ({
  type: LOADED_SCHEDULED_TRIGGERS,
  data,
});
export const setCurrentTrigger = (
  triggerName: string
): RASetCurrentTrigger => ({
  type: SET_CURRENT_TRIGGER,
  name: triggerName,
});

const reducer = (state = defaultState, action: RAEvents) => {
  switch (action.type) {
    case LOADED_SCHEDULED_TRIGGERS:
      return {
        ...state,
        triggers: {
          ...state.triggers,
          scheduled: action.data,
        },
        loading: false,
      };
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
