import defaultState from './state';
import {
  EventTrigger,
  Triggers,
  ScheduledTrigger,
  LOADED_EVENT_TRIGGERS,
  LOADED_SCHEDULED_TRIGGERS,
  LOADED_TRIGGERS,
  SET_CURRENT_TRIGGER,
  RATriggers,
} from './Types';

export const setTriggers = (data: Triggers) => ({
  type: LOADED_TRIGGERS,
  data,
});
export const setScheduledTriggers = (data: ScheduledTrigger[]) => ({
  type: LOADED_SCHEDULED_TRIGGERS,
  data,
});
export const setEventTriggers = (data: EventTrigger[]) => ({
  type: LOADED_EVENT_TRIGGERS,
  data,
});
export const setCurrentTrigger = (triggerName: string) => ({
  type: SET_CURRENT_TRIGGER,
  name: triggerName,
});

const reducer = (state = defaultState, action: RATriggers) => {
  switch (action.type) {
    case LOADED_TRIGGERS:
      return {
        ...state,
        triggers: action.data,
      };
    case LOADED_SCHEDULED_TRIGGERS:
      return {
        ...state,
        triggers: {
          ...state.triggers,
          scheduled: action.data,
        },
      };
    case LOADED_EVENT_TRIGGERS:
      return {
        ...state,
        triggers: {
          ...state.triggers,
          event: action.data,
        },
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
