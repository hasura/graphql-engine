import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import globals from '../../../Globals';
import { SERVER_CONSOLE_MODE } from '../../../constants';

import { ModifyCustomFunction, PermissionCustomFunction } from '.';

import {
  fetchDataInit,
  fetchFunctionInit,
  UPDATE_CURRENT_SCHEMA,
  updateSchemaInfo,
} from './DataActions';
import ConnectedDataPageContainer from './DataPageContainer';
import { ConnectedRightContainer } from '../../Common/Layout';
import ConnectedSchema from './Schema/Schema';
import ConnectedFunctionWrapper from './Function/FunctionWrapper';
import ConnectedViewTable from './TableBrowseRows/ViewTable';
import ConnectedEditItem from './TableBrowseRows/EditItem';
import ConnectedInsertItem from './TableInsertItem/InsertItem';
import ConnectedModifyTable from './TableModify/ModifyTable';
import ConnectedRelationships from './TableRelationships/Relationships';
import ConnectedPermissions from './TablePermissions/Permissions';
import ConnectedModifyView from './TableModify/ModifyView';
import ConnectedRelationshipsView from './TableRelationships/RelationshipsView';
import ConnectedPermissionsSummary from './PermissionsSummary/PermissionsSummary';
import ConnectedAddTable from './Add/AddTable';
import ConnectedAddExistingTableView from './Add/AddExistingTableView';
import ConnectedRawSQL from './RawSQL/RawSQL';
import ConnectedMigrations from './Migrations/Migrations';

const makeDataRouter = (
  composeOnEnterHooks,
  migrationRedirects,
  consoleModeRedirects
) => {
  console.log({ composeOnEnterHooks });
  return (
    <Route path="data" component={ConnectedDataPageContainer}>
      <IndexRedirect to="schema/public" />
      <Route path="schema" component={ConnectedRightContainer}>
        <IndexRedirect to="public" />
        <Route path=":schema" component={ConnectedSchema} />
        <Route path=":schema/tables" component={ConnectedSchema} />
        <Route path=":schema/views" component={ConnectedSchema} />
        <Route
          path=":schema/functions/:functionName"
          component={ConnectedFunctionWrapper}
        >
          <IndexRedirect to="modify" />
          <Route path="modify" component={ModifyCustomFunction} />
          <Route path="permissions" component={PermissionCustomFunction} />
        </Route>
        <Route path=":schema/tables/:table" component={ConnectedViewTable}>
          <IndexRedirect to="browse" />
          <Route path="browse" component={ConnectedViewTable} />
        </Route>
        <Route
          path=":schema/tables/:table/edit"
          component={ConnectedEditItem}
        />
        <Route
          path=":schema/tables/:table/insert"
          component={ConnectedInsertItem}
        />
        <Route
          path=":schema/tables/:table/modify"
          onEnter={migrationRedirects}
          component={ConnectedModifyTable}
        />
        <Route
          path=":schema/tables/:table/relationships"
          component={ConnectedRelationships}
        />
        <Route
          path=":schema/tables/:table/permissions"
          component={ConnectedPermissions}
          tableType={'table'}
        />
        <Route
          path=":schema/views/:table/browse"
          component={ConnectedViewTable}
        />
        <Route
          path=":schema/views/:table/modify"
          onEnter={migrationRedirects}
          component={ConnectedModifyView}
        />
        <Route
          path=":schema/views/:table/relationships"
          component={ConnectedRelationshipsView}
        />
        <Route
          path=":schema/views/:table/permissions"
          component={ConnectedPermissions}
          tableType={'view'}
        />
        <Route
          path=":schema/permissions"
          component={ConnectedPermissionsSummary}
        />
      </Route>
      <Route
        path="schema/:schema/table/add"
        onEnter={composeOnEnterHooks([migrationRedirects])}
        component={ConnectedAddTable}
      />
      <Route
        path="schema/:schema/existing-table-view/add"
        component={ConnectedAddExistingTableView}
      />
      <Route path="sql" component={ConnectedRawSQL} />
      <Route
        path="migrations"
        onEnter={composeOnEnterHooks([consoleModeRedirects])}
        component={ConnectedMigrations}
      />
    </Route>
  );
};

const dataRouterUtils = (store, composeOnEnterHooks) => {
  const requireSchema = (nextState, replaceState, cb) => {
    // check if admin secret is available in localstorage. if so use that.
    // if localstorage admin secret didn't work, redirect to login (meaning value has changed)
    // if admin secret is not available in localstorage, check if cli is giving it via window.__env
    // if admin secret is not available in localstorage and cli, make a api call to data without admin secret.
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
      store.dispatch(updateSchemaInfo()),
      store.dispatch(fetchFunctionInit()),
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
      composeOnEnterHooks,
      migrationRedirects,
      consoleModeRedirects
    ),
    requireSchema,
    migrationRedirects,
  };
};

export default dataRouterUtils;
