import { push, replace } from 'react-router-redux';
import { Thunk } from '../../../types';
import { makeMigrationCall } from '../Data/DataActions';
import requestAction from '../../../utils/requestAction';
import {
  getETModifyRoute,
  getScheduledEventsLandingRoute,
  getSTModifyRoute,
  getDataEventsLandingRoute,
  getAdhocPendingEventsRoute,
} from '../../Common/utils/routesUtils';
import { transformHeaders } from '../../Common/Headers/utils';
import {
  getConfirmation,
  isURLTemplated,
  isValidURL,
} from '../../Common/utils/jsUtils';
import Endpoints from '../../../Endpoints';
import dataHeaders from '../Data/Common/Headers';
import {
  ScheduledTrigger,
  EventTrigger,
  EventKind,
  InvocationLog,
} from './types';
import { setCurrentTrigger } from './reducer';
import { LocalScheduledTriggerState } from './CronTriggers/state';
import { LocalAdhocEventState } from './AdhocEvents/Add/state';
import {
  LocalEventTriggerState,
  parseServerETDefinition,
} from './EventTriggers/state';
import { validateAddState as validateAdhocEventState } from './AdhocEvents/utils';
import { validateETState } from './EventTriggers/utils';
import {
  validateAddState,
  parseServerScheduledTrigger,
} from './CronTriggers/utils';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../Common/Notification';
import { EventTriggerProperty } from './EventTriggers/Modify/utils';
import { getLogsTableDef } from './utils';
import { Table } from '../../../dataSources/types';
import {
  generateCreateEventTriggerQuery,
  getDropEventTriggerQuery,
  generateCreateScheduledTriggerQuery,
  generateUpdateScheduledTriggerQuery,
  getDropScheduledTriggerQuery,
  getCreateScheduledEventQuery,
  getRedeliverDataEventQuery,
  deleteScheduledEvent,
  SupportedEvents,
} from '../../../metadata/queryUtils';
import { exportMetadata } from '../../../metadata/actions';
import { getRunSqlQuery } from '../../Common/utils/v1QueryUtils';
import { QualifiedTable } from '../../../metadata/types';
import { dataSource } from '../../../dataSources';
import Migration from '../../../utils/migration/Migration';
import _push from '../Data/push';

export const addScheduledTrigger = (
  state: LocalScheduledTriggerState,
  successCb?: () => void,
  errorCb?: () => void
): Thunk => (dispatch, getState) => {
  const { currentDataSource } = getState().tables;
  const validationError = validateAddState(state);

  const errorMsg = 'Creating scheduled trigger failed';
  if (validationError) {
    if (errorCb) {
      errorCb();
    }
    return dispatch(showErrorNotification(errorMsg, validationError));
  }
  const migration = new Migration();
  migration.add(
    generateCreateScheduledTriggerQuery(state, currentDataSource),
    getDropScheduledTriggerQuery(state.name, currentDataSource)
  );

  const migrationName = `create_scheduled_trigger_${state.name}`;
  const requestMsg = 'Creating scheduled trigger...';
  const successMsg = 'Created scheduled trigger successfully';

  const customOnSuccess = () => {
    dispatch(exportMetadata())
      .then(() => {
        if (successCb) {
          successCb();
        }
        dispatch(
          push(getSTModifyRoute(encodeURIComponent(state.name), 'absolute'))
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

  return makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
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
): Thunk => (dispatch, getState) => {
  const { currentDataSource } = getState().tables;
  const validationError = validateAddState(state);

  const errorMsg = 'Updating scheduled trigger failed';
  if (validationError) {
    if (errorCb) {
      errorCb();
    }
    return dispatch(showErrorNotification(errorMsg, validationError));
  }

  const isRenamed = state.name !== existingTrigger.name;
  if (isRenamed) {
    const isOk = getConfirmation(
      'Renaming a trigger deletes the current trigger and creates a new trigger with this configuration. All the events of the current trigger will be dropped.',
      true,
      'RENAME'
    );
    if (!isOk) {
      if (errorCb) {
        errorCb();
      }
      return null;
    }
  }
  const migration = new Migration();
  if (!isRenamed) {
    migration.add(
      generateUpdateScheduledTriggerQuery(state, currentDataSource),
      generateUpdateScheduledTriggerQuery(
        parseServerScheduledTrigger(existingTrigger),
        currentDataSource
      )
    );
  } else {
    // drop existing
    migration.add(
      getDropScheduledTriggerQuery(existingTrigger.name, currentDataSource),
      generateCreateScheduledTriggerQuery(
        parseServerScheduledTrigger(existingTrigger),
        currentDataSource
      )
    );
    // create new
    migration.add(
      generateCreateScheduledTriggerQuery(state, currentDataSource),
      getDropScheduledTriggerQuery(state.name, currentDataSource)
    );
  }

  const migrationName = `update_scheduled_trigger_${existingTrigger.name}_to_${state.name}`;
  const requestMsg = 'Updating scheduled trigger...';
  const successMsg = 'Updated scheduled trigger successfully';

  const customOnSuccess = () => {
    return dispatch(exportMetadata())
      .then(() => {
        if (successCb) {
          successCb();
        }
        if (isRenamed) {
          const newHref = window.location.href.replace(
            getSTModifyRoute(
              encodeURIComponent(existingTrigger.name),
              'relative'
            ),
            getSTModifyRoute(encodeURIComponent(state.name), 'relative')
          );
          dispatch(replace(newHref));
          dispatch(setCurrentTrigger(state.name));
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

  return makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
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
): Thunk => (dispatch, getState) => {
  const isOk = getConfirmation(
    `This will delete the cron trigger permanently and delete all the associated events.`,
    true,
    trigger.name
  );
  if (!isOk) {
    if (errorCb) {
      errorCb();
    }
    return;
  }

  const { currentDataSource } = getState().tables;

  const migration = new Migration();
  migration.add(
    getDropScheduledTriggerQuery(trigger.name, currentDataSource),
    generateCreateScheduledTriggerQuery(
      parseServerScheduledTrigger(trigger),
      currentDataSource
    )
  );

  const migrationName = `delete_scheduled_trigger_${trigger.name}`;
  const requestMsg = 'Deleting scheduled trigger...';
  const errorMsg = 'Deleting scheduled trigger failed';
  const successMsg = 'Deleted scheduled trigger successfully';

  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
    dispatch(_push(getScheduledEventsLandingRoute('absolute')));
    dispatch(exportMetadata());
  };
  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    false
  );
};

export const createEventTrigger = (
  state: LocalEventTriggerState,
  successCb?: () => null,
  errorCb?: () => null
): Thunk => {
  return (dispatch, getState) => {
    const validationError = validateETState(state);
    if (validationError) {
      dispatch(
        showErrorNotification('Creating event trigger failed', validationError)
      );
      return;
    }

    const migrationName = `create_event_trigger_${state.name.trim()}`;

    const migration = new Migration();
    migration.add(
      generateCreateEventTriggerQuery(state, state.source),
      getDropEventTriggerQuery(state.name, state.source)
    );

    const requestMsg = 'Creating event trigger...';
    const successMsg = 'Event Trigger Created';
    const errorMsg = 'Creating event trigger failed';

    const customOnSuccess = () => {
      if (successCb) {
        successCb();
      }
      dispatch(exportMetadata()).then(() => {
        dispatch(_push(getETModifyRoute({ name: state.name })));
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
      migration.upMigration,
      migration.downMigration,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg,
      true
    );
  };
};

export const modifyEventTrigger = (
  property: EventTriggerProperty,
  state: LocalEventTriggerState,
  trigger: EventTrigger,
  table?: Table,
  successCb?: () => void,
  errorCb?: () => void
): Thunk => (dispatch, getState) => {
  const currentSource = getState().tables.currentDataSource;

  const downQuery = generateCreateEventTriggerQuery(
    parseServerETDefinition(trigger, table),
    currentSource,
    true
  );

  // TODO optimise redeclaration of queries
  const upQuery = generateCreateEventTriggerQuery(
    parseServerETDefinition(trigger, table),
    currentSource,
    true
  );

  const errorMsg = 'Saving failed';

  switch (property) {
    case 'webhook': {
      if (
        state.webhook.type === 'static' &&
        !(
          isValidURL(state.webhook.value) || isURLTemplated(state.webhook.value)
        )
      ) {
        return dispatch(showErrorNotification(errorMsg, 'Invalid URL'));
      }
      upQuery.args = {
        ...upQuery.args,
        webhook:
          state.webhook.type === 'static' ? state.webhook.value.trim() : null,
        webhook_from_env:
          state.webhook.type === 'env' ? state.webhook.value.trim() : null,
      };
      break;
    }
    case 'ops': {
      upQuery.args = {
        ...upQuery.args,
        insert: state.operations.insert ? { columns: '*' } : null,
        update: state.operations.update
          ? {
              columns: state.operationColumns
                .filter(c => !!c.enabled)
                .map(c => c.name),
            }
          : null,
        delete: state.operations.delete ? { columns: '*' } : null,
        enable_manual: state.operations.enable_manual,
      };
      break;
    }
    case 'retry_conf': {
      upQuery.args.retry_conf = state.retryConf;
      break;
    }
    case 'headers': {
      upQuery.args.headers = transformHeaders(state.headers);
      break;
    }
    default:
      break;
  }
  const migration = new Migration();
  migration.add(upQuery, downQuery);

  const migrationName = `set_et_${state.name.trim()}_${property}`;
  const requestMsg = 'Saving...';
  const successMsg = 'Saved';

  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
    dispatch(exportMetadata());
  };

  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  return makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    true
  );
};

export const deleteEventTrigger = (
  trigger: EventTrigger,
  successCb?: () => void,
  errorCb?: () => void
): Thunk => (dispatch, getState) => {
  const source = getState().tables.currentDataSource;

  const isOk = getConfirmation(
    `This will permanently delete the event trigger and the associated metadata`,
    true,
    trigger.name
  );
  if (!isOk) {
    return undefined;
  }

  const migration = new Migration();
  migration.add(
    getDropEventTriggerQuery(trigger.name, source),
    generateCreateEventTriggerQuery(parseServerETDefinition(trigger), source)
  );

  const migrationName = `delete_et_${trigger.name}`;

  const requestMsg = 'Deleting event trigger...';
  const successMsg = 'Deleted event trigger';
  const errorMsg = 'Deleting event trigger failed';

  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
    dispatch(_push(getDataEventsLandingRoute()));
    dispatch(exportMetadata());
  };

  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  return makeMigrationCall(
    dispatch,
    getState,
    migration.upMigration,
    migration.downMigration,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    true
  );
};

export const createScheduledEvent = (
  state: LocalAdhocEventState,
  successCb?: () => void,
  errorCb?: () => void
): Thunk => (dispatch, getState) => {
  const validationError = validateAdhocEventState(state);
  const errorMessage = 'Failed scheduling the event';
  if (validationError) {
    if (errorCb) {
      errorCb();
    }
    return dispatch(showErrorNotification(errorMessage, validationError));
  }

  const currentSource = getState().tables.currentDataSource;

  const query = getCreateScheduledEventQuery(state, currentSource);
  return dispatch(
    requestAction(
      Endpoints.metadata,
      {
        method: 'POST',
        body: JSON.stringify(query),
      },
      undefined,
      undefined,
      true,
      true
    )
  ).then(
    () => {
      if (successCb) {
        successCb();
      }
      dispatch(showSuccessNotification('Event scheduled successfully'));
      dispatch(_push(getAdhocPendingEventsRoute('absolute')));
    },
    (error: any) => {
      dispatch(showErrorNotification(errorMessage, error.message || '', error));
      if (errorCb) {
        errorCb();
      }
    }
  );
};

export const redeliverDataEvent = (
  eventId: string,
  tableDef: QualifiedTable,
  eventTriggerSource: string,
  successCb?: CallableFunction,
  errorCb?: CallableFunction
): Thunk => (dispatch, getState) => {
  const url = Endpoints.metadata;
  const payload = getRedeliverDataEventQuery(
    eventId,
    tableDef,
    eventTriggerSource
  );
  const options = {
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(payload),
  };
  return dispatch(
    requestAction(url, options, undefined, undefined, true, true)
  ).then(
    () => {
      if (successCb) {
        successCb();
      }
    },
    (error: any) => {
      if (errorCb) {
        errorCb();
      }
      dispatch(
        showErrorNotification(
          'Failed to redeliver event',
          error.message || 'unexpected',
          error
        )
      );
    }
  );
};

export const getEventLogs = (
  eventId: string,
  eventKind: EventKind,
  eventDataSource?: string,
  successCallback?: (logs: InvocationLog[]) => void,
  errorCallback?: (error: any) => void
): Thunk => (dispatch, getState) => {
  const logTableDef = getLogsTableDef(eventKind);
  const eventLogTable: QualifiedTable = {
    schema: 'hdb_catalog',
    name: 'event_log',
  };
  if (!dataSource.getEventInvocationInfoByIDSql) {
    return;
  }

  const sql = dataSource.getEventInvocationInfoByIDSql(
    logTableDef,
    eventLogTable,
    eventId
  );
  const query = getRunSqlQuery(
    sql,
    eventDataSource || getState().tables.currentDataSource
  );

  return dispatch(
    requestAction(
      Endpoints.query,
      {
        method: 'POST',
        body: JSON.stringify(query),
      },
      undefined,
      undefined,
      true,
      true
    )
  )
    .then((data: any) => {
      const allKeys = data.result[0];
      const dataRows = data.result.slice(1);
      const invocationsKeys = [
        'id',
        'event_id',
        'status',
        'created_at',
        'request',
        'response',
      ];
      const formattedData: InvocationLog[] = dataRows.reduce(
        (acc: InvocationLog[], val: any) => {
          const newObj: Record<string, any> = {};
          allKeys.forEach((key: string, idx: number) => {
            if (invocationsKeys.includes(key) && !newObj[key]) {
              newObj[key] = val[idx];
            }
          });
          if (Object.keys(newObj)) {
            return [...acc, newObj];
          }
          return acc;
        },
        []
      );
      if (successCallback) successCallback(formattedData);
      return formattedData;
    })
    .catch(err => {
      if (errorCallback) errorCallback(err);
      return null;
    });
};

export const cancelEvent = (
  type: SupportedEvents,
  id: string,
  onSuccessCallback: () => void
): Thunk => dispatch => {
  const url = Endpoints.metadata;
  const payload = deleteScheduledEvent(type, id);
  const options = {
    method: 'POST',
    body: JSON.stringify(payload),
  };
  const successText = `Successfully deleted event`;
  const errorText = 'Error in cancelling the event';

  dispatch(requestAction(url, options, successText, errorText, true, true))
    .then(() => {
      dispatch(showSuccessNotification(successText));
      onSuccessCallback();
    })
    .catch(err => {
      dispatch(showErrorNotification(errorText, err.message, err));
    });
};
