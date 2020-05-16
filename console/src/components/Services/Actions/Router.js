import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import RightContainer from '../../Common/Layout';
import ActionsContainer from './Containers/Main';
import { fetchActions } from './ServerIO';
import globals from '../../../Globals';
import ConnectedAddAction from './Add';
import ConnectedModify from './Modify';
import ConnectedCodegen from './Codegen';
import ConnectedManage from './Types/Manage';
import ConnectedRelationships from './Types/Relationships';
import ConnectedActionRelationships from './Relationships';
import ConnectedActionPermissions from './Permissions';
import ConnectedLanding from './Landing/';

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
      component={ActionsContainer}
      onEnter={composeOnEnterHooks([actionsInit(store)])}
      onChange={actionsInit(store)}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={RightContainer}>
        <IndexRedirect to="actions" />
        <Route path="actions" component={ConnectedLanding} />
        <Route path="add" component={ConnectedAddAction} />
        <Route path=":actionName/modify" component={ConnectedModify} />
        <Route
          path=":actionName/relationships"
          component={ConnectedActionRelationships}
        />
        <Route path=":actionName/codegen" component={ConnectedCodegen} />
        <Route
          path=":actionName/permissions"
          component={ConnectedActionPermissions}
        />
      </Route>
      <Route path="types" component={RightContainer}>
        <IndexRedirect to="manage" />
        <Route path="manage" component={ConnectedManage} />
        <Route path="relationships" component={ConnectedRelationships} />
      </Route>
    </Route>
  );
};

export default getActionsRouter;
