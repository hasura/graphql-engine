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
} from '.';
import { rightContainerConnector } from '../../Common/Layout';
import {
  fetchDataInit,
  fetchFunctionInit,
  UPDATE_CURRENT_SCHEMA,
  updateSchemaInfo,
  fetchSchemaList,
} from './DataActions';
import {
  showErrorNotification,
  showInfoNotification,
} from '../Common/Notification';

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
      <IndexRedirect to="schema/public" />
      <Route path="schema" component={rightContainerConnector(connect)}>
        <IndexRedirect to="public" />
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
      </Route>
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
      {/*
      <Route path="metadata" component={metadataConnector(connect)} />
      */}
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
    // check if admin secret is available in localstorage. if so use that.
    // if localstorage admin secret didn't work, redirect to login (meaning value has changed)
    // if admin secret is not available in localstorage, check if cli is giving it via window.__env
    // if admin secret is not available in localstorage and cli, make a api call to data without admin secret.
    // if the api fails, then redirect to login - this is a fresh user/browser flow
    store.dispatch(fetchSchemaList()).then(() => {
      const {
        tables: { schemaList, allSchemas, currentSchema: prevSchema },
      } = store.getState();

      let currentSchema = nextState.params.schema;
      if (currentSchema && prevSchema === currentSchema && allSchemas.length) {
        return cb();
      }

      if (!currentSchema) {
        if (schemaList.map(s => s.schema_name).includes('public')) {
          currentSchema = 'public';
        } else if (schemaList.length) {
          // select new currentSchema from schemaList
          currentSchema = schemaList[0].schema_name;
          if (nextState.location.pathname.includes('data')) {
            store.dispatch(
              showInfoNotification(
                `No public schema, showing ${currentSchema} schema instead`
              )
            );
            // redirect to current schema instead of public
            replaceState(`/data/schema/${currentSchema}`);
          }
        } else {
          store.dispatch(showErrorNotification('No schema available'));
          replaceState('/');
        }
      }

      Promise.all([
        store.dispatch({
          type: UPDATE_CURRENT_SCHEMA,
          currentSchema: currentSchema,
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
