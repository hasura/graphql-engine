import { Route, Navigate } from 'react-router';
import { RightContainerRoute } from '../../components/Common/Layout/RightContainer';
import RemoteSchemaRelationships from './components/Relationships';
import RemoteSchemaDetails from './components/Details/RemoteSchemaDetails';
import RemoteSchemaLanding from './components/Landing/RemoteSchema';
import RemoteSchemaGuard from './components/RemoteSchemaGuard';
import CreateRemoteSchema from './components/CreateRemoteSchema';
import UpdateRemoteSchema from './components/UpdateRemoteSchema';
import RemoteSchemaPageContainer from './components/Container/RemoteSchemaPageContainer';
import RemoteSchemaPermissions from './components/Permissions';

const getRemoteSchemaRoutes = () => {
  return (
    <Route path="remote-schemas" element={<RemoteSchemaPageContainer />}>
      <Route index element={<Navigate to="manage" replace />} />
      <Route path="manage" element={<RightContainerRoute />}>
        <Route index element={<Navigate to="schemas" replace />} />
        <Route path="schemas" element={<RemoteSchemaLanding />} />
        <Route path="add" element={<CreateRemoteSchema />} />
        <Route path=":remoteSchemaName" element={<RemoteSchemaGuard />}>
          <Route index element={<Navigate to="details" />} />
          <Route path="details" element={<RemoteSchemaDetails />} />
          <Route path="modify" element={<UpdateRemoteSchema />} />
          <Route path="permissions" element={<RemoteSchemaPermissions />} />
          <Route path="relationships" element={<RemoteSchemaRelationships />} />
        </Route>
      </Route>
    </Route>
  );
};

export default getRemoteSchemaRoutes;
