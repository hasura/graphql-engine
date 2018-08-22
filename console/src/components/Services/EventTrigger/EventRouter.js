import React from 'react';
// import {push} fropm 'react-router-redux';
import { Route, IndexRedirect } from 'react-router';
import globals from '../../../Globals';
import { loadAccessKeyState } from '../../AppState';

import {
  schemaConnector,
  schemaContainerConnector,
  viewTableConnector,
  addTableConnector,
  eventHeaderConnector,
} from '.';

import {
  loadSchema,
  loadUntrackedSchema,
  fetchSchemaList,
  UPDATE_CURRENT_SCHEMA,
  UPDATE_DATA_HEADERS,
  ACCESS_KEY_ERROR,
} from '../EventTrigger/EventActions';

import { changeRequestHeader } from '../../ApiExplorer/Actions';
import { validateLogin } from '../../Main/Actions';

const makeEventRouter = (
  connect,
  store,
  composeOnEnterHooks,
  requireSchema,
  migrationRedirects
) => {
  return (
    <Route path="events" component={eventHeaderConnector(connect)}>
      <IndexRedirect to="manage" />
      <Route path="manage" component={schemaContainerConnector(connect)}>
        <IndexRedirect to="triggers" />
        <Route path="triggers" component={schemaConnector(connect)} />
        <Route
          path="triggers/:trigger/processed"
          component={viewTableConnector(connect)}
        />
        <Route
          path="triggers/:trigger/pending"
          component={viewTableConnector(connect)}
        />
        <Route
          path="triggers/:trigger/settings"
          component={viewTableConnector(connect)}
        />
        <Route
          path="triggers/add"
          onEnter={composeOnEnterHooks([migrationRedirects])}
          component={addTableConnector(connect)}
        />
      </Route>
    </Route>
  );
};

const eventRouter = (connect, store, composeOnEnterHooks) => {
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
    // assume cli provides access key
    let finalAccessKey = globals.accessKey;
    const localStorageAccessKey = loadAccessKeyState('CONSOLE_ACCESS_KEY');
    // check if accessKey is in localstorage
    if (localStorageAccessKey !== null) {
      // localstorage has the access key
      globals.accessKey = localStorageAccessKey;
      finalAccessKey = localStorageAccessKey;
    }
    // if access key is available, update the headers
    if (
      finalAccessKey !== '' &&
      finalAccessKey !== undefined &&
      finalAccessKey !== null
    ) {
      Promise.all([
        store.dispatch({
          type: UPDATE_DATA_HEADERS,
          data: {
            'Content-Type': 'application/json',
            'X-Hasura-Access-Key': finalAccessKey,
          },
        }),
        store.dispatch(
          changeRequestHeader(1, 'key', 'X-Hasura-Access-Key', true)
        ),
        store.dispatch(changeRequestHeader(1, 'value', finalAccessKey, true)),
      ]);
    }
    // redirect to login page if error in access key
    if (store.getState().tables.accessKeyError) {
      replaceState(globals.urlPrefix + '/login');
      cb();
    } else {
      // validate login
      store.dispatch(validateLogin(true)).then(
        () => {
          Promise.all([
            store.dispatch({
              type: UPDATE_CURRENT_SCHEMA,
              currentSchema: currentSchema,
            }),
            store.dispatch(fetchSchemaList()),
            store.dispatch(loadSchema()),
            store.dispatch(loadUntrackedSchema()),
          ]).then(
            () => {
              cb();
            },
            () => {
              // alert('Could not load schema.');
              replaceState(globals.urlPrefix);
              cb();
            }
          );
        },
        error => {
          console.error(JSON.stringify(error));
          Promise.all([
            store.dispatch({ type: ACCESS_KEY_ERROR, data: true }),
          ]).then(() => {
            replaceState('/login');
            cb();
          });
        }
      );
    }
  };
  const migrationRedirects = (nextState, replaceState, cb) => {
    const state = store.getState();
    if (!state.main.migrationMode) {
      replaceState(globals.urlPrefix + '/data/schema');
      cb();
    }
    cb();
  };
  const consoleModeRedirects = (nextState, replaceState, cb) => {
    if (globals.consoleMode === 'hasuradb') {
      replaceState(globals.urlPrefix + '/data/schema');
      cb();
    }
    cb();
  };
  return {
    makeEventRouter: makeEventRouter(
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

export default eventRouter;
