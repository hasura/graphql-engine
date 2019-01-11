import React from 'react';
// import {push} fropm 'react-router-redux';
import { Route, IndexRedirect } from 'react-router';
import globals from '../../../Globals';
// import { loadAccessKeyState } from '../../AppState';
import { SERVER_CONSOLE_MODE } from '../../../constants';

import {
  schemaConnector,
  schemaContainerConnector,
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
  dataHeaderConnector,
  migrationsConnector,
  // metadataConnector,
} from '.';

import {
  fetchDataInit,
  UPDATE_CURRENT_SCHEMA,
  // UPDATE_DATA_HEADERS,
  // ACCESS_KEY_ERROR,
} from './DataActions';

// import { changeRequestHeader } from '../../ApiExplorer/Actions';
// import { validateLogin } from '../../Main/Actions';

const makeDataRouter = (
  connect,
  store,
  composeOnEnterHooks,
  requireSchema,
  migrationRedirects,
  consoleModeRedirects
) => {
  return (
    <Route path="data" component={dataHeaderConnector(connect)}>
      <IndexRedirect to="schema/public" />
      <Route path="schema" component={schemaContainerConnector(connect)}>
        <IndexRedirect to="public" />
        <Route path=":schema" component={schemaConnector(connect)} />
        <Route path=":schema/tables" component={schemaConnector(connect)} />
        <Route path=":schema/views" component={schemaConnector(connect)} />
        <Route
          path=":schema/tables/:table/browse"
          component={viewTableConnector(connect)}
        />
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

const dataRouter = (connect, store, composeOnEnterHooks) => {
  const requireSchema = (nextState, replaceState, cb) => {
    // check if access key is available in localstorage. if so use that.
    // if localstorage access key didn't work, redirect to login (meaning value has changed)
    // if access key is not available in localstorage, check if cli is giving it via window.__env
    // if access key is not available in localstorage and cli, make a api call to data without access key.
    // if the api fails, then redirect to login - this is a fresh user/browser flow
    const {
      tables: { allSchemas },
    } = store.getState();
    if (allSchemas.length) {
      cb();
      return;
    }
    let currentSchema = nextState.params.schema;
    if (
      currentSchema === null ||
      currentSchema === undefined ||
      currentSchema === ''
    ) {
      currentSchema = 'public';
    }
    Promise.all([
      store.dispatch({
        type: UPDATE_CURRENT_SCHEMA,
        currentSchema: currentSchema,
      }),
      store.dispatch(fetchDataInit()),
    ]).then(
      () => {
        cb();
      },
      () => {
        // alert('Could not load schema.');
        replaceState('/');
        cb();
      }
    );
  };
  const migrationRedirects = (nextState, replaceState, cb) => {
    const state = store.getState();
    if (!state.main.migrationMode) {
      replaceState('/data/schema');
      cb();
    }
    cb();
  };
  const consoleModeRedirects = (nextState, replaceState, cb) => {
    if (globals.consoleMode === SERVER_CONSOLE_MODE) {
      replaceState('/data/schema');
      cb();
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

export default dataRouter;
