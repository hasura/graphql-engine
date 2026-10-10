import { Navigate, Route } from 'react-router';
import CreateRestView from './components/CreateRestView';
import { RestEndpointDetailsPage } from './components/RestEndpointDetails';
import { RestEndpointList } from './components/RestEndpointList';
import ModifyRest from './components/ModifyRest';

const getRestRoutes = () => {
  return (
    <Route path="rest">
      <Route index element={<Navigate to="list" replace />} />
      <Route path="create" element={<CreateRestView />} />
      <Route path="list" element={<RestEndpointList />} />
      <Route path="details/:name" element={<RestEndpointDetailsPage />} />
      <Route path="edit/:name" element={<ModifyRest />} />
    </Route>
  );
};

export default getRestRoutes;
