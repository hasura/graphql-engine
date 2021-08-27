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
  FunctionPermissions,
  ConnectedDatabaseManagePage,
  ConnectedCreateDataSourcePage,
} from '.';

import { exportMetadata } from '../../../metadata/actions';
import ConnectedDataSourceContainer from './DataSourceContainer';
import ConnectDatabase from './DataSources/ConnectDatabase';
import { setDriver } from '../../../dataSources';
import { UPDATE_CURRENT_DATA_SOURCE } from './DataActions';
import { getSourcesFromMetadata } from '../../../metadata/selector';

const makeDataRouter = (
  connect,
  store,
  composeOnEnterHooks,
  requireSource,
  migrationRedirects,
  consoleModeRedirects
) => {
  return (
    <Route path="data" component={dataPageConnector(connect)}>
      <Route
        path="migrations"
        onEnter={composeOnEnterHooks([consoleModeRedirects])}
        component={migrationsConnector(connect)}
      />
      <IndexRedirect to="manage" />
      <Route path="manage" component={ConnectedDatabaseManagePage} />
      <Route path="schema/manage" component={ConnectedDatabaseManagePage} />
      <Route path="sql" component={rawSQLConnector(connect)} />
      <Route path="manage/connect" component={ConnectDatabase} />
      <Route path="manage/create" component={ConnectedCreateDataSourcePage} />
      <Route path="schema/manage/connect" component={ConnectDatabase} />
      <Route path="manage/edit/:databaseName" component={ConnectDatabase} />
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
            <Route path="permissions" component={FunctionPermissions} />
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
      </Route>
    </Route>
  );
};

const dataRouterUtils = (connect, store, composeOnEnterHooks) => {
  const requireSource = (nextState, replaceState, cb) => {
    store.dispatch(exportMetadata()).then(state => {
      const sources = getSourcesFromMetadata(state) || [];
      const currentSource = state?.tables?.currentDataSource || '';

      if (currentSource) {
        return cb();
      }

      let source;
      let driver;
      if (sources.length) {
        source = sources[0].name;
        driver = sources[0].kind;
        setDriver(driver);
        store.dispatch({
          type: UPDATE_CURRENT_DATA_SOURCE,
          source: source,
        });
      }

      return cb();
    });
  };

  const migrationRedirects = (nextState, replaceState, cb) => {
    const state = store.getState();
    if (!state.main.migrationMode) {
      replaceState('/data');
    }
    cb();
  };

  const consoleModeRedirects = (nextState, replaceState, cb) => {
    if (globals.consoleMode === SERVER_CONSOLE_MODE) {
      replaceState('/data');
    }
    cb();
  };

  return {
    makeDataRouter: makeDataRouter(
      connect,
      store,
      composeOnEnterHooks,
      requireSource,
      migrationRedirects,
      consoleModeRedirects
    ),
    requireSource,
    migrationRedirects,
  };
};

export default dataRouterUtils;
