import React from 'react';

import { Route, IndexRedirect } from 'react-router';
import { ConnectedRightContainer } from '../../Common/Layout';
import globals from '../../../Globals';
import { fetchRemoteSchemas } from './Actions';
import ConnectedRemoteSchemaPageContainer from './RemoteSchemaPageContainer';
import ConnectedRemoteSchemaLanding from './Landing/RemoteSchema';
import ConnectedAdd from './Add/Add';
import ConnectedView from './Edit/View';
import ConnectedEdit from './Edit/Edit';

// Objective is to render list of custom remoteSchemas on the
// left nav bar.
// Custom remoteSchemas list is fetched from hdb_catalog/custom_remoteSchema
// Whenever any operation happens like add remoteSchema/delete remoteSchema, this state should update automatically.

const fetchInitialData = ({ dispatch }) => {
  return (nextState, replaceState, cb) => {
    /*
    const currState = getState();
    const dataList = currState.remoteSchemas.listData.remoteSchemas;
    if (dataList.length) {
      cb();
      return;
    }
    */

    Promise.all([dispatch(fetchRemoteSchemas())]).then(
      () => {
        cb();
      },
      () => {
        // alert('Could not load schema.');
        replaceState(globals.urlPrefix);
        cb();
      }
    );
  };
};

const getRemoteSchemaRouter = (store, composeOnEnterHooks) => {
  return (
    <Route
      path="remote-schemas"
      component={ConnectedRemoteSchemaPageContainer}
      onEnter={composeOnEnterHooks([fetchInitialData(store)])}
      onChange={fetchInitialData(store)}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={ConnectedRightContainer}>
        <IndexRedirect to="schemas" />
        <Route path="schemas" component={ConnectedRemoteSchemaLanding} />
        <Route path="add" component={ConnectedAdd} />
        <Route path=":remoteSchemaName/details" component={ConnectedView} />
        <Route path=":remoteSchemaName/modify" component={ConnectedEdit} />
      </Route>
    </Route>
  );
};

export default getRemoteSchemaRouter;
