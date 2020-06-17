/* eslint-disable @typescript-eslint/no-unused-vars */
import React from 'react';
import { Connect } from 'react-redux';
import { Route, IndexRedirect, EnterHook, RouterState } from 'react-router';
import rightContainerConnector from '../../Common/Layout/RightContainer/RightContainer';
import Container from './Container';
import Support from './Support';
import globals from '../../../Globals';
import { Dispatch, ReplaceRouterState, ReduxStore } from '../../../types';

// to do — don't use funtion to create router
const getSupportRouter = (
  _connect: Connect,
  _store: ReduxStore,
  _composeOnEnterHooks: (hooks: EnterHook[]) => EnterHook
) => {
  return (
    <Route path="support" component={Container}>
      <Route path="forum" component={Support} />
    </Route>
  );
};

export default getSupportRouter;
