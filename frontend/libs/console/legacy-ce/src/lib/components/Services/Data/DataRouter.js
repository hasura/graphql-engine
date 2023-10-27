import { IndexRedirect, IndexRoute, Redirect, Route } from 'react-router';

import { SERVER_CONSOLE_MODE } from '../../../constants';
import globals from '../../../Globals';

import {
  addExistingTableViewConnector,
  addTableConnector,
  ConnectedCreateDataSourcePage,
  ConnectedDatabaseManagePage,
  dataPageConnector,
  FunctionPermissions,
  functionWrapperConnector,
  migrationsConnector,
  ModifyCustomFunction,
  modifyViewConnector,
  permissionsConnector,
  permissionsSummaryConnector,
  rawSQLConnector,
  relationshipsConnector,
  relationshipsViewConnector,
  schemaConnector,
} from '.';
import { Connect } from '../../../features/ConnectDB';
import { ConnectUIContainer } from '../../../features/ConnectDBRedesign';
import { ConnectDatabaseRouteWrapper } from '../../../features/ConnectDBRedesign/ConnectDatabase.route';
import { ManageDatabaseRoute } from '../../../features/Data';
import { ManageTable } from '../../../features/Data/ManageTable';
import { setDriver } from '../../../dataSources';
import { exportMetadata } from '../../../metadata/actions';
import { getSourcesFromMetadata } from '../../../metadata/selector';
import { UPDATE_CURRENT_DATA_SOURCE } from './DataActions';
import ConnectedDataSourceContainer from './DataSourceContainer';
import ConnectDatabase from './DataSources/ConnectDatabase';
import { TableBrowseRowsContainer } from './TableBrowseRows/TableBrowseRowsContainer';
import { TableEditItemContainer } from './TableEditItem/TableEditItemContainer';
import { TableInsertItemContainer } from './TableInsertItem/TableInsertItemContainer';
import { ModifyTableContainer } from './TableModify/ModifyTableContainer';
import { LandingPageRoute as NativeQueries } from '../../../features/Data/LogicalModels/LandingPage/LandingPage';
import { TrackStoredProcedureRoute } from '../../../features/Data/LogicalModels/StoredProcedures/StoredProcedureWidget.route';
import { ManageFunction } from '../../../features/Data/ManageFunction/ManageFunction';
import { AddNativeQueryRoute } from '../../../features/Data/LogicalModels/AddNativeQuery';
import { NativeQueryRoute } from '../../../features/Data/LogicalModels/AddNativeQuery/NativeQueryLandingPage';
import { LogicalModelRoute } from '../../../features/Data/LogicalModels/LogicalModel/LogicalModelLandingPage';
import { ModelSummaryContainer } from './ModelSummary/ModelSummaryContainer';

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

      <Route path="v2">
        <Route path="manage">
          <Route path="connect" component={ConnectDatabaseRouteWrapper} />
          <Route path="database/add" component={ConnectUIContainer} />
          <Route path="database/edit" component={ConnectUIContainer} />
          <Route path="table" component={ManageTable}>
            <IndexRedirect to="modify" />
            <Route path=":operation" component={ManageTable} />
          </Route>
          <Route path="function" component={ManageFunction}>
            <IndexRedirect to="modify" />
            <Route path=":operation" component={ManageFunction} />
          </Route>
          <Route path="database" component={ManageDatabaseRoute} />
        </Route>
        <Route path="edit" component={Connect.EditConnection} />
      </Route>

      <Route path="manage" component={ConnectedDatabaseManagePage} />
      <Route path="model-count-summary" component={ModelSummaryContainer} />
      <Route path="schema/manage" component={ConnectedDatabaseManagePage} />
      <Route path="sql" component={rawSQLConnector(connect)} />

      <Route path="native-queries">
        <IndexRoute component={NativeQueries} />
        <Route path="create" component={AddNativeQueryRoute} />

        <Route path="logical-models">
          <IndexRoute component={NativeQueries} />
          <Redirect from=":source" to="/data/native-queries/logical-models" />
          <Route path=":source/:name">
            <IndexRedirect to="details" />
            <Route path=":tabName" component={LogicalModelRoute} />
          </Route>
        </Route>

        <Route path="stored-procedures" component={NativeQueries} />
        <Route
          path="stored-procedures/track"
          component={TrackStoredProcedureRoute}
        />
        <Route path=":source/:name" component={NativeQueryRoute}>
          <IndexRedirect to="details" />
          <Route path=":tabName" component={NativeQueryRoute} />
        </Route>
      </Route>
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
            component={TableBrowseRowsContainer}
          >
            <IndexRedirect to="browse" />
            <Route path="browse" component={TableBrowseRowsContainer} />
          </Route>
          <Route
            path=":schema/tables/:table/edit"
            component={TableEditItemContainer}
          />
          <Route
            path=":schema/tables/:table/insert"
            component={TableInsertItemContainer}
          />
          <Route
            path=":schema/tables/:table/modify"
            onEnter={migrationRedirects}
            component={ModifyTableContainer}
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
            component={TableBrowseRowsContainer}
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
