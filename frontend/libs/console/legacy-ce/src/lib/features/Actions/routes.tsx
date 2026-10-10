import { Route, Navigate } from 'react-router';
import { RightContainerRoute } from '../../components/Common/Layout/RightContainer';
import ActionsContainer from './components/Containers/ActionsContainer';
import ActionsLandingPage from './components/Landing';
import ActionPermissions from './components/Permissions';
import ModifyAction from './components/Modify';
import TypesManage from './components/Types/Manage';
import AddAction from './components/Add/Add';
import CurrentActionGuard from './components/Containers/CurrentActionGuard';
import ActionsCodegen from './components/Codegen';
import { OASGeneratorPage } from './components/OASGenerator';
import ActionRelationships from './components/Relationships';

const getActionRoutes = () => {
  return (
    <Route path="actions" element={<ActionsContainer />}>
      <Route index element={<Navigate to="manage" replace />} />
      <Route path="manage" element={<RightContainerRoute />}>
        <Route index element={<Navigate to="actions" replace />} />
        <Route path="actions" element={<ActionsLandingPage />} />
        <Route path="add" element={<AddAction />} />
        <Route path="add-oas" element={<OASGeneratorPage />} />
        <Route path=":actionName" element={<CurrentActionGuard />}>
          <Route index element={<Navigate to="modify" replace />} />
          <Route path="modify" element={<ModifyAction />} />
          <Route path="relationships" element={<ActionRelationships />} />
          <Route path="codegen" element={<ActionsCodegen />} />
          <Route path="permissions" element={<ActionPermissions />} />
        </Route>
      </Route>
      <Route path="types" element={<RightContainerRoute />}>
        <Route index element={<Navigate to="manage" replace />} />
        <Route path="manage" element={<TypesManage />} />
      </Route>
    </Route>
  );
};

export default getActionRoutes;
