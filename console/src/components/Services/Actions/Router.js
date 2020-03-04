import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import { rightContainerConnector } from '../../Common/Layout';
import Container from './Containers/Main';
import { fetchActions } from './ServerIO';
import globals from '../../../Globals';
import ActionsLandingPage from './Landing';
import ActionRelationships from './Relationships';
import ActionPermissions from './Permissions';
import ActionsCodegen from './Codegen';
import ModifyAction from './Modify';
import AddAction from './Add';
import TypesManage from './Types/Manage';
import TypesRelationships from './Types/Relationships';

const actionsInit = ({ dispatch }) => {
  return (nextState, replaceState, cb) => {
    Promise.all([dispatch(fetchActions())]).then(
      () => {
        cb();
      },
      () => {
        replaceState(globals.urlPrefix);
        cb();
      }
    );
  };
};

const getActionsRouter = (connect, store, composeOnEnterHooks) => {
  return (
    <Route
      path="actions"
      component={Container(connect)}
      onEnter={composeOnEnterHooks([actionsInit(store)])}
      onChange={actionsInit(store)}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={rightContainerConnector(connect)}>
        <IndexRedirect to="actions" />
        <Route path="actions" component={ActionsLandingPage(connect)} />
        <Route path="add" component={AddAction(connect)} />
        <Route path=":actionName/modify" component={ModifyAction(connect)} />
        <Route
          path=":actionName/relationships"
          component={ActionRelationships(connect)}
        />
        <Route path=":actionName/codegen" component={ActionsCodegen(connect)} />
        <Route
          path=":actionName/permissions"
          component={ActionPermissions(connect)}
        />
      </Route>
      <Route path="types" component={rightContainerConnector(connect)}>
        <IndexRedirect to="manage" />
        <Route path="manage" component={TypesManage(connect)} />
        <Route path="relationships" component={TypesRelationships(connect)} />
      </Route>
    </Route>
  );
};

export default getActionsRouter;
