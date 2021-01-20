import { defaultViewState } from '../DataState';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from 'utils/requestAction';
import filterReducer from './FilterActions';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Common/Notification';
import dataHeaders from '../Common/Headers';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  getBulkDeleteQuery,
  getSelectQuery,
  getDeleteQuery,
  getRunSqlQuery,
} from '../../../Common/utils/v1QueryUtils';
import { COUNT_LIMIT } from '../constants';
import {
  generateTableDef,
  dataSource,
  findTableFromRel,
} from '../../../../dataSources';

/* ****************** View actions *************/
const V_SET_DEFAULTS = 'ViewTable/V_SET_DEFAULTS';
const V_REQUEST_SUCCESS = 'ViewTable/V_REQUEST_SUCCESS';
const V_EXPAND_REL = 'ViewTable/V_EXPAND_REL';
const V_CLOSE_REL = 'ViewTable/V_CLOSE_REL';
const V_SET_ACTIVE = 'ViewTable/V_SET_ACTIVE';
const V_SET_QUERY_OPTS = 'ViewTable/V_SET_QUERY_OPTS';
const V_REQUEST_PROGRESS = 'ViewTable/V_REQUEST_PROGRESS';
const V_EXPAND_ROW = 'ViewTable/V_EXPAND_ROW';
const V_COLLAPSE_ROW = 'ViewTable/V_COLLAPSE_ROW';

const V_COUNT_REQUEST_SUCCESS = 'ViewTable/V_COUNT_REQUEST_SUCCESS';
const V_COUNT_REQUEST_ERROR = 'ViewTable/V_COUNT_REQUEST_SUCCESS';

const FETCHING_MANUAL_TRIGGER = 'ViewTable/FETCHING_MANUAL_TRIGGER';
const FETCH_MANUAL_TRIGGER_SUCCESS = 'ViewTable/FETCH_MANUAL_TRIGGER_SUCCESS';
const FETCH_MANUAL_TRIGGER_FAIL = 'ViewTable/FETCH_MANUAL_TRIGGER_SUCCESS';

/* ****************** action creators *************/

const vExpandRow = rowKey => ({
  type: V_EXPAND_ROW,
  data: rowKey,
});

const vCollapseRow = () => ({
  type: V_COLLAPSE_ROW,
});

const vSetDefaults = limit => ({ type: V_SET_DEFAULTS, limit });

const vMakeRowsRequest = () => {
  return (dispatch, getState) => {
    const {
      currentTable: originalTable,
      currentSchema,
      currentDataSource,
      view,
    } = getState().tables;

    const url = Endpoints.query;
    dispatch({ type: V_REQUEST_PROGRESS, data: true });

    const requestBody = {
      type: 'bulk',
      source: currentDataSource,
      args: [
        getSelectQuery(
          'select',
          generateTableDef(originalTable, currentSchema),
          view.query.columns,
          view.query.where,
          view.query.offset,
          view.query.limit,
          view.query.order_by,
          currentDataSource
        ),
        getRunSqlQuery(
          dataSource.getEstimateCountQuery(currentSchema, originalTable),
          currentDataSource
        ),
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
        const currentTable = getState().tables.currentTable;
        const estimatedCount =
          data.length > 1 && data[0].result > 1 && data.result[1].length
            ? data[1].result[1][0]
            : null;

        // in case table has changed before count load
        if (currentTable === originalTable) {
          Promise.all([
            dispatch({
              type: V_REQUEST_SUCCESS,
              data: data[0],
              estimatedCount:
                estimatedCount !== null
                  ? parseInt(data[1]?.result[1][0], 10)
                  : null,
            }),
            dispatch({ type: V_REQUEST_PROGRESS, data: false }),
          ]);
        }
      },
      error => {
        Promise.all([
          dispatch(
            showErrorNotification('Browse query failed!', error.error, error)
          ),
          dispatch({ type: V_REQUEST_PROGRESS, data: false }),
        ]);
      }
    );
  };
};

const vMakeCountRequest = () => {
  return (dispatch, getState) => {
    const {
      currentTable: originalTable,
      currentSchema,
      view,
      currentDataSource,
    } = getState().tables;
    const url = Endpoints.query;

    const selectQuery = getSelectQuery(
      'count',
      generateTableDef(originalTable, currentSchema),
      view.query.columns,
      view.query.where,
      view.query.offset,
      view.query.limit,
      view.query.order_by
    );

    const timeoutQuery = getRunSqlQuery(
      dataSource.getStatementTimeoutSql(2),
      currentDataSource
    );

    const requestBody = {
      type: 'bulk',
      source: currentDataSource,
      args: [timeoutQuery, selectQuery],
    };

    const options = {
      method: 'POST',
      body: JSON.stringify(requestBody),
      headers: dataHeaders(getState),
      credentials: globalCookiePolicy,
    };

    return dispatch(requestAction(url, options)).then(
      data => {
        if (data.length > 1) {
          const currentTable = getState().tables.currentTable;

          // in case table has changed before count load
          if (currentTable === originalTable) {
            dispatch({
              type: V_COUNT_REQUEST_SUCCESS,
              count: data[1].count,
            });
          }
        }
      },
      error => {
        dispatch({
          type: V_COUNT_REQUEST_ERROR,
        });

        if (!dataSource.isTimeoutError(error)) {
          dispatch(
            showErrorNotification('Count query failed!', error.error, error)
          );
        }
      }
    );
  };
};

const vMakeTableRequests = () => (dispatch, getState) => {
  dispatch(vMakeRowsRequest()).then(() => {
    const { estimatedCount } = getState().tables.view;
    if (estimatedCount > COUNT_LIMIT) {
      dispatch({
        type: V_COUNT_REQUEST_SUCCESS,
        count: estimatedCount,
        isEstimated: true,
      });
    } else {
      dispatch(vMakeCountRequest());
    }
  });
};

const deleteItem = (pkClause, tableName, tableSchema) => {
  return (dispatch, getState) => {
    const confirmMessage =
      'This will permanently delete this row from this table';
    const isOk = getConfirmation(confirmMessage);
    if (!isOk) {
      return;
    }

    const source = getState().tables.currentDataSource;

    const url = Endpoints.query;

    const reqBody = getDeleteQuery(pkClause, tableName, tableSchema, source);

    const options = {
      method: 'POST',
      body: JSON.stringify(reqBody),
      headers: dataHeaders(getState),
      credentials: globalCookiePolicy,
    };
    dispatch(requestAction(url, options)).then(
      data => {
        dispatch(vMakeTableRequests());
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

const deleteItems = (pkClauses, tableName, tableSchema) => {
  return (dispatch, getState) => {
    const confirmMessage = 'This will permanently delete rows from this table';
    const isOk = getConfirmation(confirmMessage);
    if (!isOk) {
      return;
    }
    const source = getState().tables.currentDataSource;

    const reqBody = {
      type: 'bulk',
      source,
      args: getBulkDeleteQuery(pkClauses, tableName, tableSchema, source),
    };
    const options = {
      method: 'POST',
      body: JSON.stringify(reqBody),
      headers: dataHeaders(getState),
      credentials: globalCookiePolicy,
    };
    dispatch(requestAction(Endpoints.query, options)).then(
      data => {
        const affected = data.reduce((acc, d) => acc + d.affected_rows, 0);
        dispatch(vMakeTableRequests());
        dispatch(
          showSuccessNotification('Rows deleted!', 'Affected rows: ' + affected)
        );
      },
      err => {
        dispatch(
          showErrorNotification('Deleting rows failed!', err.error, err)
        );
      }
    );
  };
};

const vExpandRel = (path, relname, pk) => {
  return dispatch => {
    // Modify the query (UI will automatically change)
    dispatch({ type: V_EXPAND_REL, path, relname, pk });
    // Make a request
    return dispatch(vMakeTableRequests());
  };
};
const vCloseRel = (path, relname) => {
  return dispatch => {
    // Modify the query (UI will automatically change)
    dispatch({ type: V_CLOSE_REL, path, relname });
    // Make a request
    return dispatch(vMakeTableRequests());
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
const viewReducer = (tableName, currentSchema, schemas, viewState, action) => {
  if (action.type.indexOf('ViewTable/FilterQuery/') === 0) {
    return {
      ...viewState,
      curFilter: filterReducer(viewState.curFilter, action),
    };
  }
  const tableSchema = schemas.find(
    x => x.table_name === tableName && x.table_schema === currentSchema
  );
  switch (action.type) {
    case V_SET_DEFAULTS:
      // check if table exists and then process.
      const currentTable = schemas.find(
        t => t.table_name === tableName && t.table_schema === currentSchema
      );
      let currentColumns = [];
      if (currentTable) {
        currentColumns = currentTable.columns.map(c => c.column_name);
      }
      return {
        ...defaultViewState,
        query: {
          columns: currentColumns,
          limit: action.limit || 10,
        },
        curFilter: {
          ...defaultViewState.curFilter,
          limit: action.limit || 10,
        },
        activePath: [tableName],
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
          schemas
        ),
        activePath: [...viewState.activePath, action.relname],
      };
    case V_CLOSE_REL:
      const _query = closeQuery(
        viewState.query,
        tableSchema,
        action.path,
        action.relname,
        schemas
      );
      return {
        ...viewState,
        query: _query,
        activePath: updateActivePathOnClose(
          viewState.activePath,
          tableName,
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
      return {
        ...viewState,
        rows: action.data,
        estimatedCount: action.estimatedCount,
      };
    case V_REQUEST_PROGRESS:
      return { ...viewState, isProgressing: action.data };
    case V_COUNT_REQUEST_SUCCESS:
      return {
        ...viewState,
        count: action.count,
        isCountEstimated: action.isEstimated === true,
      };
    case V_COUNT_REQUEST_ERROR:
      return {
        ...viewState,
        count: null,
        isCountEstimated: false,
      };
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
    case FETCHING_MANUAL_TRIGGER:
      return {
        ...viewState,
        ongoingRequest: true,
        lastError: {},
      };
    case FETCH_MANUAL_TRIGGER_SUCCESS:
      return {
        ...viewState,
        manualTriggers: action.data,
        ongoingRequest: false,
      };
    case FETCH_MANUAL_TRIGGER_FAIL:
      return {
        ...viewState,
        manualTriggers: [],
        ongoingRequest: false,
        lastError: action.data,
      };
    default:
      return viewState;
  }
};

export default viewReducer;
export {
  vSetDefaults,
  vExpandRel,
  vCloseRel,
  vExpandRow,
  vCollapseRow,
  V_SET_ACTIVE,
  deleteItem,
  deleteItems,
  vMakeTableRequests,
};
