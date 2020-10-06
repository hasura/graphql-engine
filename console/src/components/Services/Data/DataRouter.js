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
  updateSchemaInfo,
  fetchSchemaList,
} from './DataActions';
import { exportMetadata } from '../../../metadata/actions';
import ConnectedDataSourceContainer from './DataSourceContainer';

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
      <IndexRedirect to="manage" />
      <Route path="manage" component={ConnectedDatabaseManagePage} />
      <Route path="schema/manage" component={ConnectedDatabaseManagePage} />
      <Route path=":source" component={ConnectedDataSourceContainer}>
        <Route path="schema">
          <Route path=":schema" component={schemaConnector(connect)} />
          <Route path=":schema/tables" component={schemaConnector(connect)} />
          <Route path=":schema/views" component={schemaConnector(connect)} />
          <Route
            path=":schema/functions/:functionName"
            component={functionWrapperConnector(connect)}
          >
            <IndexRedirect to="modify" />
            <Route path="modify" component={ModifyCustomFunction} />
            <Route path="permissions" component={PermissionCustomFunction} />
          </Route>
          <Route
            path=":schema/tables/:table"
            component={viewTableConnector(connect)}
          >
            <IndexRedirect to="browse" />
            <Route path="browse" component={viewTableConnector(connect)} />
          </Route>
          <Route
            path=":schema/tables/:table/edit"
            component={editItemConnector(connect)}
          />
          <Route
            path=":schema/tables/:table/insert"
            component={insertItemConnector(connect)}
          />
          <Route
            path=":schema/tables/:table/modify"
            onEnter={migrationRedirects}
            component={modifyTableConnector(connect)}
          />
          <Route
            path=":schema/tables/:table/relationships"
            component={relationshipsConnector(connect)}
          />
          <Route
            path=":schema/tables/:table/permissions"
            component={permissionsConnector(connect)}
            tableType={'table'}
          />
          <Route
            path=":schema/views/:table/browse"
            component={viewTableConnector(connect)}
          />
          <Route
            path=":schema/views/:table/modify"
            onEnter={migrationRedirects}
            component={modifyViewConnector(connect)}
          />
          <Route
            path=":schema/views/:table/relationships"
            component={relationshipsViewConnector(connect)}
          />
          <Route
            path=":schema/views/:table/permissions"
            component={permissionsConnector(connect)}
            tableType={'view'}
          />
          <Route
            path=":schema/permissions"
            component={permissionsSummaryConnector(connect)}
          />
          <Route
            path=":schema/table/add"
            onEnter={composeOnEnterHooks([migrationRedirects])}
            component={addTableConnector(connect)}
          />
          <Route
            path=":schema/existing-table-view/add"
            component={addExistingTableViewConnector(connect)}
          />
        </Route>
        <Route path="sql" component={rawSQLConnector(connect)} />

        <Route
          path="migrations"
          onEnter={composeOnEnterHooks([consoleModeRedirects])}
          component={migrationsConnector(connect)}
        />
      </Route>
    </Route>
  );
};

const dataRouterUtils = (connect, store, composeOnEnterHooks) => {
  const requireSchema = (nextState, replaceState, cb) => {
    store
      .dispatch(exportMetadata())
      .then(() => store.dispatch(fetchSchemaList()))
      .then(() => {
        const {
          tables: { allSchemas, currentSchema: prevSchema },
        } = store.getState();

        const currentSchema = nextState.params.schema;
        if (
          currentSchema &&
          prevSchema === currentSchema &&
          allSchemas.length
        ) {
          return cb();
        }

        Promise.all([
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
