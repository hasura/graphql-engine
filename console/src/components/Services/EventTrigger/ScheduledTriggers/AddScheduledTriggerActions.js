import { CRON_TYPE } from './constants';

import { makeQuery } from './Actions';

const defaultState = {
  triggerName: '',
  webhookUrl: '',
  scheduleType: CRON_TYPE,
  scheduleValue: '',
};

/* Action constants */
const UPDATE_INPUT = '@scheduledTrigger/UPDATE_INPUT';
const RESET = '@scheduledTrigger/RESET';

const createScheduledTrigger = () => {
  return (dispatch, getState) => {
    const { addScheduledTrigger } = getState().scheduledTrigger;
    const {
      triggerName: name,
      webhookUrl: webhook,
      scheduleType: type,
      scheduleValue: value,
    } = addScheduledTrigger;
    const query = {
      type: 'create_scheduled_trigger',
      args: {
        name,
        webhook,
        schedule: {
          type,
          value,
        },
      },
    };
    return dispatch(makeQuery(query));
  };
};

/* Action creators */

const updateInput = (key, value) => {
  return {
    type: UPDATE_INPUT,
    data: {
      key,
      value,
    },
  };
};

const resetAdd = () => {
  return {
    type: RESET,
  };
};

const addScheduledTriggerReducer = (state = defaultState, action) => {
  switch (action.type) {
    case UPDATE_INPUT:
      return {
        ...state,
        [action.data.key]: action.data.value,
      };
    case RESET:
      return {
        ...defaultState,
      };
    default:
      return {
        ...state,
      };
  }
};

export { updateInput, createScheduledTrigger, resetAdd };

export default addScheduledTriggerReducer;
