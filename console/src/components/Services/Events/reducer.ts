import defaultState from './state';
import {
  EventTrigger,
  Triggers,
  ScheduledTrigger,
  LOADED_EVENT_TRIGGERS,
  LOADED_SCHEDULED_TRIGGERS,
  LOADED_TRIGGERS,
  SET_CURRENT_TRIGGER,
  RAEvents,
  RASetAllTriggers,
  RASetCurrentTrigger,
  RASetEventTriggers,
  RASetScheduledTriggers,
  LOADING_TRIGGERS,
} from './types';

export const setTriggers = (data: Triggers): RASetAllTriggers => ({
  type: LOADED_TRIGGERS,
  data,
});
export const setScheduledTriggers = (
  data: ScheduledTrigger[]
): RASetScheduledTriggers => ({
  type: LOADED_SCHEDULED_TRIGGERS,
  data,
});
export const setEventTriggers = (data: EventTrigger[]): RASetEventTriggers => ({
  type: LOADED_EVENT_TRIGGERS,
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
    case LOADED_TRIGGERS:
      return {
        ...state,
        triggers: action.data,
        loading: false,
      };
    case LOADED_SCHEDULED_TRIGGERS:
      return {
        ...state,
        triggers: {
          ...state.triggers,
          scheduled: action.data,
        },
        loading: false,
      };
    case LOADED_EVENT_TRIGGERS:
      return {
        ...state,
        triggers: {
          ...state.triggers,
          event: action.data,
        },
        loading: false,
      };
    case SET_CURRENT_TRIGGER:
      return {
        ...state,
        currentTrigger: action.name,
      };
    case LOADING_TRIGGERS:
      return {
        ...state,
        loading: true,
      };
    default:
      return state;
  }
};

export default reducer;
