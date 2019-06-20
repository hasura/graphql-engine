import { defaultViewState } from '../EventState';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from 'utils/requestAction';
import processedFilterReducer from './FilterActions';
import { findTableFromRel } from '../utils';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import dataHeaders from '../Common/Headers';

/* ****************** View actions *************/
const V_SET_DEFAULTS = 'ProcessedEvents/V_SET_DEFAULTS';
const V_REQUEST_SUCCESS = 'ProcessedEvents/V_REQUEST_SUCCESS';
const V_REQUEST_ERROR = 'ProcessedEvents/V_REQUEST_ERROR';
const V_EXPAND_REL = 'ProcessedEvents/V_EXPAND_REL';
const V_CLOSE_REL = 'ProcessedEvents/V_CLOSE_REL';
const V_SET_ACTIVE = 'ProcessedEvents/V_SET_ACTIVE';
const V_SET_QUERY_OPTS = 'ProcessedEvents/V_SET_QUERY_OPTS';
const V_EXPAND_ROW = 'ProcessedEvents/V_EXPAND_ROW';
const V_COLLAPSE_ROW = 'ProcessedEvents/V_COLLAPSE_ROW';
// const V_ADD_WHERE;
// const V_REMOVE_WHERE;
// const V_SET_LIMIT;
// const V_SET_OFFSET;
// const V_ADD_SORT;
// const V_REMOVE_SORT;

/* ****************** action creators *************/

const vExpandRow = rowKey => ({
  type: V_EXPAND_ROW,
  data: rowKey,
});

const vCollapseRow = () => ({
  type: V_COLLAPSE_ROW,
});

const vSetDefaults = () => ({ type: V_SET_DEFAULTS });

const vMakeRequest = () => {
  return (dispatch, getState) => {
    const state = getState();
    const url = Endpoints.query;
    const originalTrigger = getState().triggers.currentTrigger;
    const currentQuery = JSON.parse(JSON.stringify(state.triggers.view.query));
    // count query
    const countQuery = JSON.parse(JSON.stringify(state.triggers.view.query));
    countQuery.columns = ['id'];

    // delivered = true || error = true
    // where clause for relationship
    const currentWhereClause = state.triggers.view.query.where;
    if (currentWhereClause && currentWhereClause.$and) {
      // make filter for events
      const finalAndClause = currentQuery.where.$and;
      finalAndClause.push({
        $or: [{ delivered: { $eq: true } }, { error: { $eq: true } }],
      });
      currentQuery.columns[1].where = { $and: finalAndClause };
      currentQuery.where = { name: state.triggers.currentTrigger };
      countQuery.where.$and.push({
        trigger_name: originalTrigger,
      });
    } else {
      // reset where for events
      if (currentQuery.columns[1]) {
        currentQuery.columns[1].where = {
          $or: [{ delivered: { $eq: true } }, { error: { $eq: true } }],
        };
      }
      currentQuery.where = { name: state.triggers.currentTrigger };
      countQuery.where = {
        $and: [
          { trigger_name: state.triggers.currentTrigger },
          { $or: [{ delivered: { $eq: true } }, { error: { $eq: true } }] },
        ],
      };
    }

    // order_by for relationship
    const currentOrderBy = state.triggers.view.query.order_by;
    if (currentOrderBy) {
      currentQuery.columns[1].order_by = currentOrderBy;
      // reset order_by
      delete currentQuery.order_by;
    } else {
      // reset order by for events
      if (currentQuery.columns[1]) {
        delete currentQuery.columns[1].order_by;
        currentQuery.columns[1].order_by = ['-created_at'];
      }
      delete currentQuery.order_by;
    }

    // limit and offset for relationship
    const currentLimit = state.triggers.view.query.limit;
    const currentOffset = state.triggers.view.query.offset;
    currentQuery.columns[1].limit = currentLimit;
    currentQuery.columns[1].offset = currentOffset;

    // reset limit and offset for parent
    delete currentQuery.limit;
    delete currentQuery.offset;
    delete countQuery.limit;
    delete countQuery.offset;

    const requestBody = {
      type: 'bulk',
      args: [
        {
          type: 'select',
          args: {
            ...currentQuery,
            table: {
              name: 'event_triggers',
              schema: 'hdb_catalog',
            },
          },
        },
        {
          type: 'count',
          args: {
            ...countQuery,
            table: {
              name: 'event_log',
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
          ]);
        }
      },
      error => {
        dispatch({ type: V_REQUEST_ERROR, data: error });
      }
    );
  };
};

const deleteItem = pkClause => {
  return (dispatch, getState) => {
    const isOk = confirm('Permanently delete this row?');
    if (!isOk) {
      return;
    }
    const state = getState();
    const url = Endpoints.query;
    const reqBody = {
      type: 'delete',
      args: {
        table: {
          name: state.triggers.currentTrigger,
          schema: 'hdb_catalog',
        },
        where: pkClause,
      },
    };
    const options = {
      method: 'POST',
      body: JSON.stringify(reqBody),
      headers: dataHeaders(getState),
      credentials: globalCookiePolicy,
    };
    dispatch(requestAction(url, options)).then(
      data => {
        dispatch(vMakeRequest());
        dispatch(
          showSuccessNotification(
            'Row deleted!',
            'Affected rows: ' + data.affected_rows
          )
        );
      },
      err => {
        dispatch(showErrorNotification('Deleting row failed!', err.error, err));
      }
    );
  };
};

const vExpandRel = (path, relname, pk) => {
  return dispatch => {
    // Modify the query (UI will automatically change)
    dispatch({ type: V_EXPAND_REL, path, relname, pk });
    // Make a request
    return dispatch(vMakeRequest());
  };
};
const vCloseRel = (path, relname) => {
  return dispatch => {
    // Modify the query (UI will automatically change)
    dispatch({ type: V_CLOSE_REL, path, relname });
    // Make a request
    return dispatch(vMakeRequest());
  };
};
/* ************ helpers ************************/
const defaultSubQuery = (relname, tableSchema) => {
  return {
    name: relname,
    columns: tableSchema.columns.map(c => c.column_name),
  };
};

const expandQuery = (
  curQuery,
  curTable,
  pk,
  curPath,
  relname,
  schemas,
  isObjRel = false
) => {
  if (curPath.length === 0) {
    const rel = curTable.relationships.find(r => r.rel_name === relname);
    const childTableSchema = findTableFromRel(schemas, curTable, rel);

    const newColumns = [
      ...curQuery.columns,
      defaultSubQuery(relname, childTableSchema),
    ];
    if (isObjRel) {
      return { ...curQuery, columns: newColumns };
    }

    // If there's already oldStuff then don't reset it
    if ('oldStuff' in curQuery) {
      return { ...curQuery, where: pk, columns: newColumns };
    }

    // If there's no oldStuff then set it
    const oldStuff = {};
    ['where', 'limit', 'offset'].map(k => {
      if (k in curQuery) {
        oldStuff[k] = curQuery[k];
      }
    });
    return { name: curQuery.name, where: pk, columns: newColumns, oldStuff };
  }

  const curRelName = curPath[0];
  const curRel = curTable.relationships.find(r => r.rel_name === curRelName);
  const childTableSchema = findTableFromRel(schemas, curTable, curRel);
  const curRelColIndex = curQuery.columns.findIndex(c => c.name === curRelName);
  return {
    ...curQuery,
    columns: [
      ...curQuery.columns.slice(0, curRelColIndex),
      expandQuery(
        curQuery.columns[curRelColIndex],
        childTableSchema,
        pk,
        curPath.slice(1),
        relname,
        schemas,
        curRel.rel_type === 'object'
      ),
      ...curQuery.columns.slice(curRelColIndex + 1),
    ],
  };
};

const closeQuery = (curQuery, curTable, curPath, relname, schemas) => {
  // eslint-disable-line no-unused-vars
  if (curPath.length === 0) {
    const expandedIndex = curQuery.columns.findIndex(c => c.name === relname);
    const newColumns = [
      ...curQuery.columns.slice(0, expandedIndex),
      ...curQuery.columns.slice(expandedIndex + 1),
    ];
    const newStuff = {};
    newStuff.columns = newColumns;
    if ('name' in curQuery) {
      newStuff.name = curQuery.name;
    }
    // If no other expanded columns are left
    if (!newColumns.find(c => typeof c === 'object')) {
      if (curQuery.oldStuff) {
        ['where', 'limit', 'order_by', 'offset'].map(k => {
          if (k in curQuery.oldStuff) {
            newStuff[k] = curQuery.oldStuff[k];
          }
        });
      }
      return { ...newStuff };
    }
    return { ...curQuery, ...newStuff };
  }

  const curRelName = curPath[0];
  const curRel = curTable.relationships.find(r => r.rel_name === curRelName);
  const childTableSchema = findTableFromRel(schemas, curTable, curRel);
  const curRelColIndex = curQuery.columns.findIndex(c => c.name === curRelName);
  return {
    ...curQuery,
    columns: [
      ...curQuery.columns.slice(0, curRelColIndex),
      closeQuery(
        curQuery.columns[curRelColIndex],
        childTableSchema,
        curPath.slice(1),
        relname,
        schemas
      ),
      ...curQuery.columns.slice(curRelColIndex + 1),
    ],
  };
};

const setActivePath = (activePath, curPath, relname, query) => {
  const basePath = relname
    ? [activePath[0], ...curPath, relname]
    : [activePath[0], ...curPath];

  // Now check if there are any more children on this path.
  // If there are, then we should expand them by default
  let subQuery = query;
  let subBase = basePath.slice(1);

  while (subBase.length > 0) {
    subQuery = subQuery.columns.find(c => c.name === subBase[0]); // eslint-disable-line no-loop-func
    subBase = subBase.slice(1);
  }

  subQuery = subQuery.columns.find(c => typeof c === 'object');
  while (subQuery) {
    basePath.push(subQuery.name);
    subQuery = subQuery.columns.find(c => typeof c === 'object');
  }

  return basePath;
};
const updateActivePathOnClose = (
  activePath,
  tableName,
  curPath,
  relname,
  query
) => {
  const basePath = [tableName, ...curPath, relname];
  let subBase = [...basePath];
  let subActive = [...activePath];
  let matchingFound = false;
  let commonIndex = 0;
  subBase = subBase.slice(1);
  subActive = subActive.slice(1);

  while (subActive.length > 0) {
    if (subBase[0] === subActive[0]) {
      matchingFound = true;
      break;
    }
    subBase = subBase.slice(1);
    subActive = subActive.slice(1);
    commonIndex += 1;
  }

  if (matchingFound) {
    const newActivePath = activePath.slice(0, commonIndex + 1);
    return setActivePath(
      newActivePath,
      newActivePath.slice(1, -1),
      null,
      query
    );
  }
  return [...activePath];
};
const addQueryOptsActivePath = (query, queryStuff, activePath) => {
  let curPath = activePath.slice(1);
  const newQuery = { ...query };
  let curQuery = newQuery;
  while (curPath.length > 0) {
    curQuery = curQuery.columns.find(c => c.name === curPath[0]); // eslint-disable-line no-loop-func
    curPath = curPath.slice(1);
  }

  ['where', 'order_by', 'limit', 'offset'].map(k => {
    delete curQuery[k];
  });

  for (const k in queryStuff) {
    if (queryStuff.hasOwnProperty(k)) {
      curQuery[k] = queryStuff[k];
    }
  }
  return newQuery;
};
/* ****************** reducer ******************/
const processedEventsReducer = (
  triggerName,
  triggerList,
  viewState,
  action
) => {
  if (action.type.indexOf('ProcessedEvents/FilterQuery/') === 0) {
    return {
      ...viewState,
      curFilter: processedFilterReducer(viewState.curFilter, action),
    };
  }
  const tableSchema = triggerList.find(x => x.name === triggerName);
  switch (action.type) {
    case V_SET_DEFAULTS:
      // check if table exists and then process.
      /*
      const currentTrigger = triggerList.find(t => t.name === triggerName);
      let currentColumns = [];
      if (currentTrigger) {
        currentColumns = currentTrigger.map(c => c.column_name);
      }
      */
      return {
        ...defaultViewState,
        query: {
          columns: [
            '*',
            {
              name: 'events',
              columns: [
                '*',
                { name: 'logs', columns: ['*'], order_by: ['-created_at'] },
              ],
              where: {
                $or: [{ delivered: { $eq: true } }, { error: { $eq: true } }],
              },
            },
          ],
          limit: 10,
        },
        activePath: [triggerName],
        rows: [],
        count: null,
      };
    case V_SET_QUERY_OPTS:
      return {
        ...viewState,
        query: addQueryOptsActivePath(
          viewState.query,
          action.queryStuff,
          viewState.activePath
        ),
      };
    case V_EXPAND_REL:
      return {
        ...viewState,
        query: expandQuery(
          viewState.query,
          tableSchema,
          action.pk,
          action.path,
          action.relname,
          triggerList
        ),
        activePath: [...viewState.activePath, action.relname],
      };
    case V_CLOSE_REL:
      const _query = closeQuery(
        viewState.query,
        tableSchema,
        action.path,
        action.relname,
        triggerList
      );
      return {
        ...viewState,
        query: _query,
        activePath: updateActivePathOnClose(
          viewState.activePath,
          triggerName,
          action.path,
          action.relname,
          _query
        ),
      };
    case V_SET_ACTIVE:
      return {
        ...viewState,
        activePath: setActivePath(
          viewState.activePath,
          action.path,
          action.relname,
          viewState.query
        ),
      };
    case V_REQUEST_SUCCESS:
      return { ...viewState, rows: action.data, count: action.count };
    case V_EXPAND_ROW:
      return {
        ...viewState,
        expandedRow: action.data,
      };
    case V_COLLAPSE_ROW:
      return {
        ...viewState,
        expandedRow: '',
      };
    default:
      return viewState;
  }
};

export default processedEventsReducer;
export {
  vSetDefaults,
  vMakeRequest,
  vExpandRel,
  vCloseRel,
  vExpandRow,
  vCollapseRow,
  V_SET_ACTIVE,
  deleteItem,
};
