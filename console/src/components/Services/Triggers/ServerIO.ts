import {
  fetchEventTriggersQuery,
  fetchScheduledTriggersQuery,
  getBulkQuery,
  generateCreateScheduledTriggerQuery,
  getDropScheduledTriggerQuery,
  generateUpdateScheduledTriggerQuery,
} from '../../Common/utils/v1QueryUtils';
import globals from '../../../Globals';
import { makeMigrationCall } from '../Data/DataActions';
import requestAction from '../../../utils/requestAction';
import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import dataHeaders from '../Data/Common/Headers';
import {
  TriggerKind,
  SCHEDULED_TRIGGER_IDENTIFIER,
  ScheduledTrigger,
  EventTrigger,
} from './Types';
import { setScheduledTriggers, setEventTriggers, setTriggers } from './reducer';
import { LocalScheduledTriggerState } from './ScheduledTriggers/Add/state';
import {
  validateAddState,
  parseServerScheduledTrigger,
} from './ScheduledTriggers/utils';
import { showErrorNotification } from '../Common/Notification';
import { push } from 'react-router-redux';
import { appPrefix } from './constants';

export const fetchTriggers = (kind?: TriggerKind) => (
  dispatch: any,
  getState: any
) => {
  const bulkQueryArgs = [];
  if (kind) {
    bulkQueryArgs.push(
      kind === SCHEDULED_TRIGGER_IDENTIFIER
        ? fetchScheduledTriggersQuery
        : fetchEventTriggersQuery
    );
  } else {
    bulkQueryArgs.push(fetchEventTriggersQuery, fetchScheduledTriggersQuery);
  }

  return dispatch(
    requestAction(Endpoints.getSchema, {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
      body: JSON.stringify(getBulkQuery(bulkQueryArgs)),
    })
  ).then(
    (data: Array<ScheduledTrigger[] | EventTrigger[]>) => {
      if (kind) {
        if (kind === SCHEDULED_TRIGGER_IDENTIFIER) {
          dispatch(setScheduledTriggers(data[0] as ScheduledTrigger[]));
        } else {
          dispatch(setEventTriggers(data[0] as EventTrigger[]));
        }
      } else {
        dispatch(
          setTriggers({
            event: data[0] as EventTrigger[],
            scheduled: data[1] as ScheduledTrigger[],
          })
        );
      }
      return Promise.resolve();
    },
    (error: any) => {
      console.error('Failed to load event triggers' + JSON.stringify(error));
      return Promise.reject();
    }
  );
};

export const addScheduledTrigger = (
  state: LocalScheduledTriggerState,
  successCb?: () => void,
  errorCb?: () => void
) => (dispatch: any, getState: any) => {
  const validationError = validateAddState(state);

  const errorMsg = 'Creating scheduled trigger failed';
  if (validationError) {
    if (errorCb) {
      errorCb();
    }
    return dispatch(showErrorNotification(errorMsg, validationError));
  }

  const upQuery = generateCreateScheduledTriggerQuery(state);
  const downQuery = getDropScheduledTriggerQuery(state.name);

  const migrationName = `create_scheduled_trigger_${state.name}`;
  const requestMsg = 'Creating scheduled trigger...';
  const successMsg = 'Created scheduled trigger successfully';

  const customOnSuccess = () => {
    dispatch(fetchTriggers(SCHEDULED_TRIGGER_IDENTIFIER))
      .then(() => {
        if (successCb) {
          successCb();
        }
        dispatch(
          push(
            `${globals.urlPrefix}${appPrefix}/scheduled/${state.name}/modify`
          )
        );
      })
      .catch(() => {
        if (errorCb) {
          errorCb();
        }
      });
  };
  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  makeMigrationCall(
    dispatch,
    getState,
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    false
  );
};

export const saveScheduledTrigger = (
  state: LocalScheduledTriggerState,
  existingTrigger: ScheduledTrigger,
  successCb?: () => void,
  errorCb?: () => void
) => (dispatch: any, getState: any) => {
  const validationError = validateAddState(state);

  const errorMsg = 'Updating scheduled trigger failed';
  if (validationError) {
    if (errorCb) {
      errorCb();
    }
    return dispatch(showErrorNotification(errorMsg, validationError));
  }

  const upQuery = generateUpdateScheduledTriggerQuery(state);
  const downQuery = generateUpdateScheduledTriggerQuery(
    parseServerScheduledTrigger(existingTrigger)
  );

  const migrationName = `update_scheduled_trigger_${existingTrigger.name}_to_${state.name}`;
  const requestMsg = 'Updating scheduled trigger...';
  const successMsg = 'Updated scheduled trigger successfully';

  const customOnSuccess = () => {
    if (state.name !== existingTrigger.name) {
      const newHref = window.location.href.replace(
        `${existingTrigger.name}/modify`,
        `${state.name}/modify`
      );
      return window.location.replace(newHref);
    }
    dispatch(fetchTriggers(SCHEDULED_TRIGGER_IDENTIFIER))
      .then(() => {
        if (successCb) {
          successCb();
        }
      })
      .catch(() => {
        if (errorCb) {
          errorCb();
        }
      });
  };
  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  makeMigrationCall(
    dispatch,
    getState,
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    false
  );
};

export const deleteScheduledTrigger = (
  trigger: ScheduledTrigger,
  successCb?: () => void,
  errorCb?: () => void
) => (dispatch: any, getState: any) => {
  const upQuery = getDropScheduledTriggerQuery(trigger.name);
  const downQuery = generateCreateScheduledTriggerQuery(
    parseServerScheduledTrigger(trigger)
  );

  const migrationName = `delete_scheduled_trigger_${trigger.name}`;
  const requestMsg = 'Deleting scheduled trigger...';
  const errorMsg = 'Deleting scheduled trigger failed';
  const successMsg = 'Deleted scheduled trigger successfully';

  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
    dispatch(push(`${globals.urlPrefix}${appPrefix}/scheduled/manage`));
    dispatch(fetchTriggers(SCHEDULED_TRIGGER_IDENTIFIER))
      .then(() => {
        dispatch(push(`${globals.urlPrefix}${appPrefix}/scheduled/manage`));
      })
      .catch(() => {
        if (errorCb) {
          errorCb();
        }
      });
  };
  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  makeMigrationCall(
    dispatch,
    getState,
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    false
  );
};
