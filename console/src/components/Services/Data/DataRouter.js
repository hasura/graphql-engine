import React from 'react';
import { Route, IndexRedirect } from 'react-router';

import globals from '../../../Globals';
import { SERVER_CONSOLE_MODE } from '../../../constants';

import {
  schemaConnector,
  viewTableConnector,
  insertItemConnector,
  rawSQLConnector,
  editItemConnector,
  addExistingTableViewConnector,
  addTableConnector,
  modifyTableConnector,
  modifyViewConnector,
  relationshipsConnector,
  relationshipsViewConnector,
  permissionsConnector,
  dataPageConnector,
  migrationsConnector,
  functionWrapperConnector,
  permissionsSummaryConnector,
  ModifyCustomFunction,
  PermissionCustomFunction,
  ConnectedDatabaseManagePage,
} from '.';

import {
  fetchDataInit,
  fetchFunctionInit,
  UPDATE_CURRENT_SCHEMA,
  updateSchemaInfo,
  fetchSchemaList,
  UPDATE_CURRENT_DATA_SOURCE,
} from './DataActions';
import { showInfoNotification } from '../Common/Notification';
import { getInitDataSource } from '../../../metadata/selector';
import { setDriver } from '../../../dataSources';

const makeDataRouter = (
  connect,
  store,
  composeOnEnterHooks,
  requireSchema,
  migrationRedirects,
  consoleModeRedirects
) => {
  return (
    <Route path="data" component={dataPageConnector(connect)}>
      <IndexRedirect to="schema" />
      <Route path="schema" component={schemaConnector(connect)} />
      <Route path="manage" component={ConnectedDatabaseManagePage} />
      <Route path="schema/:schema" component={schemaConnector(connect)} />
      <Route
        path="schema/:schema/tables"
        component={schemaConnector(connect)}
      />
      <Route path="schema/:schema/views" component={schemaConnector(connect)} />
      <Route
        path="schema/:schema/functions/:functionName"
        component={functionWrapperConnector(connect)}
      >
        <IndexRedirect to="modify" />
        <Route path="modify" component={ModifyCustomFunction} />
        <Route path="permissions" component={PermissionCustomFunction} />
      </Route>
      <Route
        path="schema/:schema/tables/:table"
        component={viewTableConnector(connect)}
      >
        <IndexRedirect to="browse" />
        <Route path="browse" component={viewTableConnector(connect)} />
      </Route>
      <Route
        path="schema/:schema/tables/:table/edit"
        component={editItemConnector(connect)}
      />
      <Route
        path="schema/:schema/tables/:table/insert"
        component={insertItemConnector(connect)}
      />
      <Route
        path="schema/:schema/tables/:table/modify"
        onEnter={migrationRedirects}
        component={modifyTableConnector(connect)}
      />
      <Route
        path="schema/:schema/tables/:table/relationships"
        component={relationshipsConnector(connect)}
      />
      <Route
        path="schema/:schema/tables/:table/permissions"
        component={permissionsConnector(connect)}
        tableType={'table'}
      />
      <Route
        path="schema/:schema/views/:table/browse"
        component={viewTableConnector(connect)}
      />
      <Route
        path=":schema/views/:table/modify"
        onEnter={migrationRedirects}
        component={modifyViewConnector(connect)}
      />
      <Route
        path="schema/:schema/views/:table/relationships"
        component={relationshipsViewConnector(connect)}
      />
      <Route
        path="schema/:schema/views/:table/permissions"
        component={permissionsConnector(connect)}
        tableType={'view'}
      />
      <Route
        path="schema/:schema/permissions"
        component={permissionsSummaryConnector(connect)}
      />
      <Route
        path="schema/:schema/table/add"
        onEnter={composeOnEnterHooks([migrationRedirects])}
        component={addTableConnector(connect)}
      />
      <Route
        path="schema/:schema/existing-table-view/add"
        component={addExistingTableViewConnector(connect)}
      />
      <Route path="sql" component={rawSQLConnector(connect)} />
      <Route path="manage" component={ConnectedDatabaseManagePage} />
      <Route
        path="migrations"
        onEnter={composeOnEnterHooks([consoleModeRedirects])}
        component={migrationsConnector(connect)}
      />
    </Route>
  );
};

const dataRouterUtils = (connect, store, composeOnEnterHooks) => {
  const requireSchema = (nextState, replaceState, cb) => {
    store.dispatch(fetchSchemaList()).then(() => {
      // todo: probably need to add export metadata as well
      const {
        tables: { schemaList, allSchemas, currentSchema: prevSchema },
        metadata,
      } = store.getState();

      const { source, driver } = getInitDataSource(store.getState());
      setDriver(driver);

      console.log({ source, metadata });

      let currentSchema = nextState.params.schema;
      if (currentSchema && prevSchema === currentSchema && allSchemas.length) {
        return cb();
      }

      if (!currentSchema) {
        if (schemaList.find(s => s.schema_name === 'public')) {
          currentSchema = 'public';
        } else if (schemaList.length) {
          // select new currentSchema from schemaList
          currentSchema = schemaList[0].schema_name;
          if (
            /^data|data\/schema|data\/[^\w+]/.test(nextState.location.pathname)
          ) {
            store.dispatch(
              showInfoNotification(
                `No public schema, showing ${currentSchema} schema instead`
              )
            );
            // redirect to current schema instead of public
            replaceState(`/data/schema/${currentSchema}`);
          }
        } else {
          currentSchema = '';
        }
      }

      Promise.all([
        store.dispatch({
          type: UPDATE_CURRENT_SCHEMA,
          currentSchema: currentSchema,
        }),
        store.dispatch({
          type: UPDATE_CURRENT_DATA_SOURCE,
          source,
        }),
        store.dispatch(fetchDataInit()),
        store.dispatch(updateSchemaInfo()),
        store.dispatch(fetchFunctionInit()),
      ]).then(cb, () => {
        // alert('Could not load schema.');
        replaceState('/');
        cb();
      });
    });
  };

  const migrationRedirects = (nextState, replaceState, cb) => {
    const state = store.getState();
    if (!state.main.migrationMode) {
      replaceState('/data/schema');
    }
    cb();
  };

  const consoleModeRedirects = (nextState, replaceState, cb) => {
    if (globals.consoleMode === SERVER_CONSOLE_MODE) {
      replaceState('/data/schema');
    }
    cb();
  };

  return {
    makeDataRouter: makeDataRouter(
      connect,
      store,
      composeOnEnterHooks,
      requireSchema,
      migrationRedirects,
      consoleModeRedirects
    ),
    requireSchema,
    migrationRedirects,
  };
};

export default dataRouterUtils;
