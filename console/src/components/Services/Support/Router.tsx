import React from 'react';
import { Connect } from 'react-redux';
import { Route, IndexRedirect, EnterHook, RouterState } from 'react-router';
import rightContainerConnector from '../../Common/Layout/RightContainer/RightContainer';
import Container from './Container';
import Support from './Support';
import globals from '../../../Globals';
import { Dispatch, ReplaceRouterState, ReduxStore } from '../../../types';

const getSupportRouter = (
  connect: Connect,
  store: ReduxStore,
  composeOnEnterHooks: (hooks: EnterHook[]) => EnterHook
) => {
  return (
    <Route
      path={'support'}
      component={Container}
    >
      <Route
      path = {'forum'}
      component={Support}
      />
    </Route>
  );
};

export default getSupportRouter;
