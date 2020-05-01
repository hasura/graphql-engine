import { push } from 'react-router-redux';
import {
  fetchEventTriggersQuery,
  fetchScheduledTriggersQuery,
  getBulkQuery,
  generateCreateScheduledTriggerQuery,
  getDropScheduledTriggerQuery,
  generateUpdateScheduledTriggerQuery,
  generateCreateEventTriggerQuery,
  getDropEventTriggerQuery,
} from '../../Common/utils/v1QueryUtils';
import globals from '../../../Globals';
import { makeMigrationCall } from '../Data/DataActions';
import requestAction from '../../../utils/requestAction';
import {
  getETModifyRoute,
  getDataEventsLandingRoute,
} from '../../Common/utils/routesUtils';
import { transformHeaders } from '../../Common/Headers/utils';
import { Table } from '../../Common/utils/pgUtils';
import { getConfirmation, isValidURL } from '../../Common/utils/jsUtils';
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
  LocalEventTriggerState,
  parseServerETDefinition,
} from './EventTriggers/state';
import { validateAddETState } from './EventTriggers/utils';
import {
  validateAddState,
  parseServerScheduledTrigger,
} from './ScheduledTriggers/utils';
import { showErrorNotification } from '../Common/Notification';
import { appPrefix } from './constants';
import { EventTriggerProperty } from './EventTriggers/Modify/utils';

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
      console.error(`Failed to load event triggers${JSON.stringify(error)}`);
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

  return makeMigrationCall(
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
    return dispatch(fetchTriggers(SCHEDULED_TRIGGER_IDENTIFIER))
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

  return makeMigrationCall(
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

export const createEventTrigger = (
  state: LocalEventTriggerState,
  successCb?: () => null,
  errorCb?: () => null
) => {
  return (dispatch: any, getState: any) => {
    const validationError = validateAddETState(state);
    if (validationError) {
      dispatch(
        showErrorNotification('Creating event trigger failed', validationError)
      );
    }

    const migrationName = `create_event_trigger_${state.name.trim()}`;

    const upQuery = generateCreateEventTriggerQuery(state);
    const downQuery = getDropEventTriggerQuery(state.name);

    const requestMsg = 'Creating event trigger...';
    const successMsg = 'Event Trigger Created';
    const errorMsg = 'Creating event trigger failed';

    const customOnSuccess = () => {
      if (successCb) {
        successCb();
      }
      dispatch(fetchTriggers('event')).then(() => {
        dispatch(push(getETModifyRoute()));
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
) => (dispatch: any, getState: any) => {
  const downQuery = generateCreateEventTriggerQuery(
    parseServerETDefinition(trigger, table),
    true
  );

  // TODO optimise redeclaration of queries

  const upQuery = generateCreateEventTriggerQuery(
    parseServerETDefinition(trigger, table),
    true
  );

  const errorMsg = 'Saving failed';

  switch (property) {
    case 'webhook': {
      if (state.webhook.type === 'static' && !isValidURL(state.webhook.value)) {
        return dispatch(showErrorNotification(errorMsg, 'Invalid URL'));
      }
      upQuery.args = {
        ...upQuery.args,
        webhook: state.webhook.type === 'static' ? state.webhook.value : null,
        webhook_from_env:
          state.webhook.type === 'env' ? state.webhook.value : null,
      };
      break;
    }
    case 'ops': {
      upQuery.args = {
        ...upQuery.args,
        insert: state.operations.insert ? { columns: '*' } : null,
        update: state.operations.update
          ? {
              columns: state.operationColumns.map(c => c.name),
              payload: state.operationColumns.map(c => c.name),
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

  const migrationName = `set_et_${state.name.trim()}_${property}`;

  const requestMsg = 'Saving...';
  const successMsg = 'Saved';

  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
    dispatch(fetchTriggers('event'));
  };

  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  return makeMigrationCall(
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
    true
  );
};

export const deleteEventTrigger = (
  trigger: EventTrigger,
  successCb?: () => void,
  errorCb?: () => void
) => (dispatch: any, getState: any) => {
  const isOk = getConfirmation(
    `This will permanently delete the event trigger and the associated metadata`,
    true,
    trigger.name
  );
  if (!isOk) {
    return undefined;
  }

  const upQuery = getDropEventTriggerQuery(trigger.name);
  const downQuery = generateCreateEventTriggerQuery(
    parseServerETDefinition(trigger)
  );

  const migrationName = `delete_et_${trigger.name}`;

  const requestMsg = 'Deleting event trigger...';
  const successMsg = 'Deleted event trigger';
  const errorMsg = 'Deleting event trigger failed';

  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
    dispatch(push(getDataEventsLandingRoute()));
    dispatch(fetchTriggers('event'));
  };

  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  return makeMigrationCall(
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
    true
  );
};
// const loadPendingDataEvents = () => (dispatch: any, getState: any) => {
//   const url = Endpoints.getSchema;
//   const body = {
//     type: 'select',
//     args: {
//       table: {
//         name: 'event_triggers',
//         schema: 'hdb_catalog',
//       },
//       columns: [
//         '*',
//         {
//           name: 'events',
//           columns: [
//             '*',
//             { name: 'logs', columns: ['*'], order_by: ['-created_at'] },
//           ],
//           where: { delivered: false, error: false, tries: 0, archived: false },
//           order_by: ['-created_at'],
//           limit: 10,
//         },
//       ],
//     },
//   };

//   const options = {
//     method: 'POST',
//     headers: dataHeaders(getState),
//     body: JSON.stringify(body),
//   };
//   return dispatch(requestAction(url, options)).then(
//     (data: any) => {
//       dispatch({ type: LOAD_PENDING_DATA_EVENTS, data: data });
//     },
//     (error: any) => {
//       console.error('Failed to load triggers' + JSON.stringify(error));
//     }
//   );
// };

// const loadRunningDataEvents = () => (dispatch, getState) => {
//   const url = Endpoints.getSchema;
//   const body = {
//     type: 'select',
//     args: {
//       table: {
//         name: 'event_triggers',
//         schema: 'hdb_catalog',
//       },
//       columns: [
//         '*',
//         {
//           name: 'events',
//           columns: [
//             '*',
//             { name: 'logs', columns: ['*'], order_by: ['-created_at'] },
//           ],
//           where: {
//             delivered: false,
//             error: false,
//             tries: { $gt: 0 },
//             archived: false,
//           },
//           order_by: ['-created_at'],
//           limit: 10,
//         },
//       ],
//     },
//   };

//   const options = {
//     credentials: globalCookiePolicy,
//     method: 'POST',
//     headers: dataHeaders(getState),
//     body: JSON.stringify(body),
//   };
//   return dispatch(requestAction(url, options)).then(
//     data => {
//       dispatch({ type: LOAD_RUNNING_EVENTS, data: data });
//     },
//     error => {
//       console.error('Failed to load triggers' + JSON.stringify(error));
//     }
//   );
// };

// const loadDataEventLogs = triggerName => (dispatch, getState) => {
//   const url = Endpoints.getSchema;
//   const triggerOptions = {
//     method: 'POST',
//     headers: dataHeaders(getState),
//     body: JSON.stringify({
//       type: 'select',
//       args: {
//         table: {
//           name: 'event_triggers',
//           schema: 'hdb_catalog',
//         },
//         columns: ['*'],
//         where: {
//           name: triggerName,
//         },
//       },
//     }),
//   };
//   return dispatch(requestAction(url, triggerOptions)).then(
//     triggerData => {
//       if (triggerData.length !== 0) {
//         const body = {
//           type: 'bulk',
//           args: [
//             {
//               type: 'select',
//               args: {
//                 table: {
//                   name: 'event_invocation_logs',
//                   schema: 'hdb_catalog',
//                 },
//                 columns: [
//                   '*',
//                   {
//                     name: 'event',
//                     columns: ['*'],
//                   },
//                 ],
//                 where: {
//                   event: { trigger_name: triggerData[0].name, archived: false },
//                 },
//                 order_by: ['-created_at'],
//                 limit: 10,
//               },
//             },
//           ],
//         };

//         const logOptions = {
//           credentials: globalCookiePolicy,
//           method: 'POST',
//           headers: dataHeaders(getState),
//           body: JSON.stringify(body),
//         };
//         dispatch(requestAction(url, logOptions)).then(
//           logsData => {
//             dispatch({ type: LOAD_EVENT_LOGS, data: logsData[0] });
//           },
//           error => {
//             console.error(
//               'Failed to load trigger logs' + JSON.stringify(error)
//             );
//           }
//         );
//       } else {
//         dispatch(replace('/404'));
//       }
//     },
//     error => {
//       console.error(
//         'Failed to fetch trigger information' + JSON.stringify(error)
//       );
//     }
//   );
// }

// const loadDataEventInvocations = eventId => (dispatch, getState) => {
//   const url = Endpoints.getSchema;
//   const options = {
//     credentials: globalCookiePolicy,
//     method: 'POST',
//     headers: dataHeaders(getState),
//     body: JSON.stringify({
//       type: 'select',
//       args: {
//         table: {
//           name: 'event_invocation_logs',
//           schema: 'hdb_catalog',
//         },
//         columns: [
//           '*',
//           {
//             name: 'event',
//             columns: ['*'],
//           },
//         ],
//         where: { event_id: eventId },
//         order_by: ['-created_at'],
//       },
//     }),
//   };
//   return dispatch(requestAction(url, options)).then(
//     data => {
//       dispatch({ type: LOAD_EVENT_INVOCATIONS, data: data });
//     },
//     error => {
//       console.error('Failed to load triggers' + JSON.stringify(error));
//     }
//   );
// }

// const redeliverEvent = eventId => (dispatch, getState) => {
//   const url = Endpoints.getSchema;
//   const options = {
//     credentials: globalCookiePolicy,
//     method: 'POST',
//     headers: dataHeaders(getState),
//     body: JSON.stringify({
//       type: 'redeliver_event',
//       args: {
//         event_id: eventId,
//       },
//     }),
//   };
//   return dispatch(requestAction(url, options)).then(
//     data => {
//       dispatch({ type: REDELIVER_EVENT_SUCCESS, data: data });
//     },
//     error => {
//       console.error('Failed to load triggers' + JSON.stringify(error));
//       dispatch({ type: REDELIVER_EVENT_FAILURE, data: error });
//     }
//   );
// }
