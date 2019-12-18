import { CRON_TYPE, ONE_OFF_TYPE } from './constants';

import { makeQuery } from './Actions';

import { showErrorNotification } from '../../Common/Notification';

const defaultState = {
  triggerName: '',
  webhookUrl: '',
  scheduleType: CRON_TYPE,
  scheduleValue: '',
  payload: '',
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
      payload,
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
        payload,
      },
    };
    if (query.args.schedule.type === ONE_OFF_TYPE) {
      try {
        query.args.schedule.value = new Date(
          query.args.schedule.value
        ).toISOString();
      } catch (e) {
        dispatch(showErrorNotification('Error creating trigger', e.toString()));
        return;
      }
    }
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
