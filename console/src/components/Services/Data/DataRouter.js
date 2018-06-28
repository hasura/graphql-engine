import React from 'react';
// import {push} fropm 'react-router-redux';
import { Route, IndexRedirect } from 'react-router';

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
} from '.';

import {
  loadSchema,
  loadUntrackedSchema,
  fetchSchemaList,
  UPDATE_CURRENT_SCHEMA,
} from './DataActions';

const makeDataRouter = (
  connect,
  store,
  composeOnEnterHooks,
  requireSchema,
  migrationRedirects
) => {
  return (
    <Route
      path="data"
      component={dataHeaderConnector(connect)}
      onEnter={composeOnEnterHooks([requireSchema])}
    >
      <IndexRedirect to="schema/public" />
      <Route
        path="schema"
        component={schemaContainerConnector(connect)}
        onEnter={requireSchema}
      >
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
        onEnter={composeOnEnterHooks([requireSchema, migrationRedirects])}
        component={addTableConnector(connect)}
      />
      <Route
        path="schema/:schema/existing-table-view/add"
        onEnter={requireSchema}
        component={addExistingTableViewConnector(connect)}
      />
      <Route
        path="sql"
        onEnter={requireSchema}
        component={rawSQLConnector(connect)}
      />
      <Route
        path="migrations"
        onEnter={requireSchema}
        component={migrationsConnector(connect)}
      />
    </Route>
  );
};

const dataRouter = (connect, store, composeOnEnterHooks) => {
  const requireSchema = (nextState, replaceState, cb) => {
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
      store.dispatch(fetchSchemaList()),
      store.dispatch(loadSchema()),
      store.dispatch(loadUntrackedSchema()),
    ]).then(
      () => {
        cb();
      },
      () => {
        alert('Could not load schema.');
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
  return {
    makeDataRouter: makeDataRouter(
      connect,
      store,
      composeOnEnterHooks,
      requireSchema,
      migrationRedirects
    ),
    requireSchema,
    migrationRedirects,
  };
};

export default dataRouter;
