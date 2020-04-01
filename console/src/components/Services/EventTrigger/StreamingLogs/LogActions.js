import { defaultLogState } from '../EventState';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from 'utils/requestAction';
import dataHeaders from '../Common/Headers';

/* ****************** View actions *************/
const V_SET_DEFAULTS = 'StreamingLogs/V_SET_DEFAULTS';
const V_REQUEST_SUCCESS = 'StreamingLogs/V_REQUEST_SUCCESS';
const V_REQUEST_NEWER_SUCCESS = 'StreamingLogs/V_REQUEST_NEWER_SUCCESS';
const V_REQUEST_OLDER_SUCCESS = 'StreamingLogs/V_REQUEST_OLDER_SUCCESS';
const V_REQUEST_ERROR = 'StreamingLogs/V_REQUEST_ERROR';
const V_REQUEST_PROGRESS = 'StreamingLogs/V_REQUEST_PROGRESS';
const V_IS_LOADING_OLDER = 'StreamingLogs/V_IS_LOADING_OLDER';
const V_IS_LOADING_NEWER = 'StreamingLogs/V_IS_LOADING_NEWER';
const V_OLD_AVAILABLE = 'StreamingLogs/V_OLD_AVAILABLE';
const V_NEW_AVAILABLE = 'StreamingLogs/V_NEW_AVAILABLE';

/* ****************** action creators *************/

const vSetDefaults = () => ({ type: V_SET_DEFAULTS });
const toggleLoadingOlder = status => ({
  type: V_IS_LOADING_OLDER,
  data: status,
});
const toggleLoadingNewer = status => ({
  type: V_IS_LOADING_NEWER,
  data: status,
});
const toggleOldAvailable = status => ({ type: V_OLD_AVAILABLE, data: status });
const toggleNewAvailable = status => ({ type: V_NEW_AVAILABLE, data: status });

const vMakeRequest = triggerName => {
  return (dispatch, getState) => {
    const state = getState();
    const url = Endpoints.query;
    const originalTrigger = getState().triggers.currentTrigger;
    dispatch({ type: V_REQUEST_PROGRESS, data: true });
    const currentQuery = JSON.parse(JSON.stringify(state.triggers.log.query));
    // count query
    const countQuery = JSON.parse(JSON.stringify(state.triggers.log.query));
    countQuery.columns = ['id'];

    currentQuery.where = {
      event: { trigger_name: triggerName, archived: false },
    };
    countQuery.where.event.archived = false;

    // order_by for relationship
    currentQuery.order_by = ['-created_at'];

    const requestBody = {
      type: 'bulk',
      args: [
        {
          type: 'select',
          args: {
            ...currentQuery,
            table: {
              name: 'event_invocation_logs',
              schema: 'hdb_catalog',
            },
          },
        },
        {
          type: 'count',
          args: {
            ...countQuery,
            table: {
              name: 'event_invocation_logs',
              schema: 'hdb_catalog',
            },
          },
        },
      ],
    };
    const options = {
      method: 'POST',
      body: JSON.stringify(requestBody),
      headers: dataHeaders(getState),
      credentials: globalCookiePolicy,
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        const currentTrigger = getState().triggers.currentTrigger;
        if (originalTrigger === currentTrigger) {
          Promise.all([
            dispatch({
              type: V_REQUEST_SUCCESS,
              data: data[0],
              count: data[1].count,
            }),
            dispatch({ type: V_REQUEST_PROGRESS, data: false }),
          ]);
        }
      },
      error => {
        dispatch({ type: V_REQUEST_ERROR, data: error });
      }
    );
  };
};

const loadNewerEvents = (latestTimestamp, triggerName) => {
  return (dispatch, getState) => {
    const state = getState();
    const url = Endpoints.query;
    dispatch({ type: V_REQUEST_PROGRESS, data: true });
    const currentQuery = JSON.parse(JSON.stringify(state.triggers.log.query));
    // count query
    const countQuery = JSON.parse(JSON.stringify(state.triggers.log.query));
    countQuery.columns = ['id'];

    currentQuery.where = {
      event: { trigger_name: triggerName, archived: false },
      created_at: { $gt: latestTimestamp },
    };

    countQuery.where.event.archived = false;

    // order_by for relationship
    currentQuery.order_by = ['-created_at'];

    const requestBody = {
      type: 'bulk',
      args: [
        {
          type: 'select',
          args: {
            ...currentQuery,
            table: {
              name: 'event_invocation_logs',
              schema: 'hdb_catalog',
            },
          },
        },
        {
          type: 'count',
          args: {
            ...countQuery,
            table: {
              name: 'event_invocation_logs',
              schema: 'hdb_catalog',
            },
          },
        },
      ],
    };
    const options = {
      method: 'POST',
      body: JSON.stringify(requestBody),
      headers: dataHeaders(getState),
      credentials: globalCookiePolicy,
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        const currentTrigger = getState().triggers.currentTrigger;
        if (triggerName === currentTrigger) {
          if (data && data[0] && data[0].length === 0) {
            dispatch({
              type: V_NEW_AVAILABLE,
              data: false,
            });
          } else {
            dispatch({
              type: V_NEW_AVAILABLE,
              data: true,
            });
          }
          Promise.all([
            dispatch({
              type: V_REQUEST_NEWER_SUCCESS,
              data: data[0],
              count: data[1].count,
            }),
            dispatch({ type: V_REQUEST_PROGRESS, data: false }),
            dispatch({ type: V_IS_LOADING_NEWER, data: false }),
          ]).then(() => {
            const trGroup = document.getElementsByClassName('rt-tr-group');
            const finalTrGroup = trGroup[0];
            finalTrGroup.scrollIntoView({
              behavior: 'smooth',
              block: 'end',
              inline: 'nearest',
            });
          });
        }
      },
      error => {
        dispatch({ type: V_REQUEST_ERROR, data: error });
      }
    );
  };
};

const loadOlderEvents = (oldestTimestamp, triggerName) => {
  return (dispatch, getState) => {
    const state = getState();
    const url = Endpoints.query;
    dispatch({ type: V_REQUEST_PROGRESS, data: true });
    const currentQuery = JSON.parse(JSON.stringify(state.triggers.log.query));
    // count query
    const countQuery = JSON.parse(JSON.stringify(state.triggers.log.query));
    countQuery.columns = ['id'];

    currentQuery.where = {
      event: { trigger_name: triggerName, archived: false },
      created_at: { $lt: oldestTimestamp },
    };

    countQuery.where.event.archived = false;

    // order_by for relationship
    currentQuery.order_by = ['-created_at'];

    const requestBody = {
      type: 'bulk',
      args: [
        {
          type: 'select',
          args: {
            ...currentQuery,
            table: {
              name: 'event_invocation_logs',
              schema: 'hdb_catalog',
            },
          },
        },
        {
          type: 'count',
          args: {
            ...countQuery,
            table: {
              name: 'event_invocation_logs',
              schema: 'hdb_catalog',
            },
          },
        },
      ],
    };
    const options = {
      method: 'POST',
      body: JSON.stringify(requestBody),
      headers: dataHeaders(getState),
      credentials: globalCookiePolicy,
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        const currentTrigger = getState().triggers.currentTrigger;
        if (triggerName === currentTrigger) {
          if (data && data[0] && data[0].length === 0) {
            dispatch({
              type: V_OLD_AVAILABLE,
              data: false,
            });
          } else {
            dispatch({
              type: V_OLD_AVAILABLE,
              data: true,
            });
          }
          Promise.all([
            dispatch({
              type: V_REQUEST_OLDER_SUCCESS,
              data: data[0],
              count: data[1].count,
            }),
            dispatch({ type: V_REQUEST_PROGRESS, data: false }),
            dispatch({ type: V_IS_LOADING_OLDER, data: false }),
          ]).then(() => {
            const trGroup = document.getElementsByClassName('rt-tr-group');
            const finalTrGroup = trGroup[trGroup.length - 1];
            finalTrGroup.scrollIntoView({
              behavior: 'smooth',
              block: 'end',
              inline: 'nearest',
            });
          });
        }
      },
      error => {
        dispatch({ type: V_REQUEST_ERROR, data: error });
      }
    );
  };
};

/* ****************** reducer ******************/
const streamingLogsReducer = (triggerName, triggerList, logState, action) => {
  switch (action.type) {
    case V_SET_DEFAULTS:
      return {
        ...defaultLogState,
        query: {
          columns: [
            '*',
            {
              name: 'event',
              columns: ['*'],
            },
          ],
          limit: 10,
          where: { event: { trigger_name: triggerName } },
        },
        activePath: [triggerName],
        rows: [],
        count: null,
      };
    case V_REQUEST_SUCCESS:
      const currentRows = logState.rows;
      const newResult = action.data;
      const combinedRows = currentRows.length
        ? newResult.concat(currentRows)
        : newResult;
      return { ...logState, rows: combinedRows, count: action.count };
    case V_REQUEST_NEWER_SUCCESS:
      const existingRows = logState.rows;
      const newRows = action.data;
      const finalRows = newRows.concat(existingRows);
      return { ...logState, rows: finalRows, count: finalRows.count };
    case V_REQUEST_OLDER_SUCCESS:
      const _existingRows = logState.rows;
      const oldRows = action.data;
      const _finalRows = _existingRows.concat(oldRows);
      return { ...logState, rows: _finalRows, count: _finalRows.count };
    case V_REQUEST_PROGRESS:
      return { ...logState, isProgressing: action.data };
    case V_IS_LOADING_NEWER:
      return { ...logState, isLoadingNewer: action.data };
    case V_IS_LOADING_OLDER:
      return { ...logState, isLoadingOlder: action.data };
    case V_OLD_AVAILABLE:
      return { ...logState, isOldAvailable: action.data };
    case V_NEW_AVAILABLE:
      return { ...logState, isNewAvailable: action.data };
    default:
      return logState;
  }
};

export default streamingLogsReducer;
export {
  vSetDefaults,
  vMakeRequest,
  loadNewerEvents,
  loadOlderEvents,
  toggleLoadingNewer,
  toggleLoadingOlder,
  toggleOldAvailable,
  toggleNewAvailable,
};
