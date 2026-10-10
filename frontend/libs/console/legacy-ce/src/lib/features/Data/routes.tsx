import { Route, Navigate } from 'react-router';
import { Connect } from '../ConnectDB';
import { ConnectUIContainer } from '../ConnectDBRedesign';
import { ConnectDatabaseRouteWrapper } from '../ConnectDBRedesign/ConnectDatabase.route';
import { ManageTable } from './ManageTable';
import ConnectedDataSourceContainer from './components/DataSourceContainer';
import { LandingPageRoute as NativeQueries } from './LogicalModels/LandingPage/LandingPage';
import { TrackStoredProcedureRoute } from './LogicalModels/StoredProcedures/StoredProcedureWidget.route';
import { ManageFunction } from './ManageFunction/ManageFunction';
import { AddNativeQueryRoute } from './LogicalModels/AddNativeQuery';
import { NativeQueryRoute } from './LogicalModels/AddNativeQuery/NativeQueryLandingPage';
import { LogicalModelRoute } from './LogicalModels/LogicalModel/LogicalModelLandingPage';
import { ModelSummaryContainer } from './ModelSummary/ModelSummaryContainer';
import { PermissionSummary } from './ManageDatabase/parts/PermissionSummary';
import Migrations from '../Migrations/Migrations';
import CurrentTableProvider from './components/CurrentTableProvider';
import RawSQL from './RawSQL/RawSQL';
import { AddTable } from './AddTable';
import ConnectedDatabaseManagePage from '../ConnectDB/ConnectedDatabaseManage';
import {
  ManageDatabaseRedirect,
  AddTableRedirect,
  ConnectUIContainerRedirect,
  ManageFunctionRedirect,
  ManageTableRedirect,
} from './Legacy';
import { ManageDatabase } from './ManageDatabase/ManageDatabase';
import { ManageDatabaseRoute } from './ManageDatabase/ManageDatabase.Route';
import DataPageContainer from './components/DataPageContainer';

const makeDataRouter = () => {
  return (
    <Route path="data" element={<DataPageContainer />}>
      <Route path="migrations" element={<Migrations />} />
      <Route index element={<Navigate to="manage" replace />} />
      {/* Deprecated: merged to v1 */}
      <Route path="v2">
        <Route path="manage">
          <Route index element={<Navigate to="/data/manage" replace />} />
          <Route
            path="connect"
            element={<Navigate to="/data/manage/connect" replace />}
          />
          <Route path="database" element={<ManageDatabaseRedirect />} />
          <Route
            path="database/add"
            element={<ConnectUIContainerRedirect action="add" />}
          />
          <Route
            path="database/edit"
            element={<ConnectUIContainerRedirect action="edit" />}
          />
          <Route path="table">
            <Route index element={<ManageTableRedirect />} />
            <Route path=":operation" element={<ManageTableRedirect />} />
          </Route>
          <Route path="function" element={<ManageFunction />}>
            <Route index element={<Navigate to="modify" replace />} />
            <Route path=":operation" element={<ManageFunction />} />
          </Route>
        </Route>
        <Route path="edit" element={<Navigate to="/data/edit" replace />} />
      </Route>

      {/* Merge data routes v2 and v1 */}
      <Route path="edit" element={<Connect.EditConnection />} />
      <Route path="manage">
        <Route index element={<ConnectedDatabaseManagePage />} />
        <Route path="connect" element={<ConnectDatabaseRouteWrapper />} />
        <Route path="database/add" element={<ConnectUIContainer />} />
        <Route path="database/edit" element={<ConnectUIContainer />} />
        <Route path="source/:source" element={<ManageDatabaseRoute />}>
          <Route index element={<ManageDatabase />} />
          <Route path="table/add" element={<AddTable />} />
          <Route path="table" element={<ManageTable />}>
            <Route index element={<Navigate to="modify" replace />} />
            <Route path=":operation" element={<ManageTable />} />
          </Route>
          <Route path="function" element={<ManageFunction />}>
            <Route index element={<Navigate to="modify" replace />} />
            <Route path=":operation" element={<ManageFunction />} />
          </Route>
          <Route path="permission-summary" element={<PermissionSummary />} />
        </Route>
      </Route>
      <Route path="model-count-summary" element={<ModelSummaryContainer />} />
      <Route path="schema/manage" element={<ConnectedDatabaseManagePage />} />
      <Route path="sql" element={<RawSQL />} />

      <Route path="native-queries">
        <Route index element={<NativeQueries />} />
        <Route path="create" element={<AddNativeQueryRoute />} />

        <Route path="logical-models">
          <Route index element={<NativeQueries />} />
          <Route
            path=":source"
            element={
              <Navigate to="/data/native-queries/logical-models" replace />
            }
          />
          <Route path=":source/:name">
            <Route index element={<Navigate to="details" replace />} />
            <Route path=":tabName" element={<LogicalModelRoute />} />
          </Route>
        </Route>

        <Route path="stored-procedures" element={<NativeQueries />} />
        <Route
          path="stored-procedures/track"
          element={<TrackStoredProcedureRoute />}
        />
        <Route path=":source/:name" element={<NativeQueryRoute />}>
          <Route index element={<Navigate to="details" replace />} />
          <Route path=":tabName" element={<NativeQueryRoute />} />
        </Route>
      </Route>
      {/* Legacy routes. Remove them later */}
      <Route
        path=":source/schema/:schema"
        element={<ConnectedDataSourceContainer />}
      >
        <Route index element={<ManageDatabaseRedirect />} />
        <Route path="tables" element={<ManageDatabaseRedirect />} />
        <Route path="views" element={<ManageDatabaseRedirect />} />
        <Route path="functions/:functionName">
          <Route index element={<ManageFunctionRedirect />} />
          <Route path=":operation" element={<ManageFunctionRedirect />} />
        </Route>
        <Route path="tables/:table" element={<CurrentTableProvider />}>
          <Route index element={<ManageTableRedirect />} />
          <Route path=":operation" element={<ManageTableRedirect />} />
        </Route>
        <Route path="views/:table">
          <Route index element={<ManageTableRedirect />} />
          <Route path=":operation" element={<ManageTableRedirect />} />
        </Route>
        <Route
          path="permissions"
          element={<ManageDatabaseRedirect subroute="permission-summary" />}
        />
        <Route path="table/add" element={<AddTableRedirect />} />
      </Route>
    </Route>
  );
};

export default makeDataRouter;
