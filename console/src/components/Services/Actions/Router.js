import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import { rightContainerConnector } from '../../Common/Layout';
import Container from './Containers/Main';
import { fetchActions } from './ServerIO';
import globals from '../../../Globals';
import ActionDetails from './Details';
import ActionsLandingPage from './Landing';
import ActionRelationships from './Relationships';
import ActionPermissions from './Permissions';
import ModifyAction from './Modify';
import AddAction from './Add';
import { fetchCustomTypes } from '../Types/ServerIO';

const fetchInitialData = ({ dispatch }) => {
  return (nextState, replaceState, cb) => {
    Promise.all([
      dispatch(fetchCustomTypes()).then(() => {
        dispatch(fetchActions());
      }),
    ]).then(
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
      onEnter={composeOnEnterHooks([fetchInitialData(store)])}
      onChange={fetchInitialData(store)}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={rightContainerConnector(connect)}>
        <IndexRedirect to="actions" />
        <Route path="actions" component={ActionsLandingPage(connect)} />
        <Route path="add" component={AddAction(connect)} />
        <Route path=":actionName/details" component={ActionDetails(connect)} />
        <Route path=":actionName/modify" component={ModifyAction(connect)} />
        <Route
          path=":actionName/relationships"
          component={ActionRelationships(connect)}
        />
        <Route
          path=":actionName/permissions"
          component={ActionPermissions(connect)}
        />
      </Route>
    </Route>
  );
};

export default getActionsRouter;
