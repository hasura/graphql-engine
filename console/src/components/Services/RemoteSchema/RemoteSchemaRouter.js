import React from 'react';

import { Route, IndexRedirect } from 'react-router';
import { rightContainerConnector } from '../../Common/Layout';
import globals from '../../../Globals';
import {
  remoteSchemaPageConnector,
  landingConnector,
  addConnector,
  editConnector,
  viewConnector,
} from '.';
import { fetchRemoteSchemas, FILTER_REMOTE_SCHEMAS } from './Actions';

// Objective is to render list of custom remoteSchemas on the
// left nav bar.
// Custom remoteSchemas list is fetched from hdb_catalog/custom_remoteSchema
// Whenever any operation happens like add remoteSchema/delete remoteSchema, this state should update automatically.

import { appPrefix } from './constants';

const filterItem = dispatch => {
  return (dataList, searchVal) => {
    // form new schema
    const matchedTables = dataList.filter(data => {
      return (
        data.name
          .toLowerCase()
          .indexOf(searchVal ? searchVal.toLowerCase() : '') !== -1
      );
    });
    dispatch({
      type: FILTER_REMOTE_SCHEMAS,
      data: {
        filtered: matchedTables,
        searchQuery: searchVal,
      },
    });
  };
};

const leftNavMapStateToProps = state => {
  return {
    ...state,
    dataList: [...state.remoteSchemas.listData.remoteSchemas],
    isError: state.remoteSchemas.listData.isError,
    isRequesting: state.remoteSchemas.listData.isRequesting,
    filtered: [...state.remoteSchemas.listData.filtered],
    searchQuery: state.remoteSchemas.listData.searchQuery,
    viewRemoteSchema: state.remoteSchemas.listData.viewRemoteSchema,
    appPrefix,
  };
};

const leftNavMapDispatchToProps = dispatch => {
  return {
    filterItem: filterItem(dispatch),
  };
};

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

const getRemoteSchemaRouter = (connect, store, composeOnEnterHooks) => {
  return (
    <Route
      path="remote-schemas"
      component={remoteSchemaPageConnector(
        connect,
        leftNavMapStateToProps,
        leftNavMapDispatchToProps
      )}
      onEnter={composeOnEnterHooks([fetchInitialData(store)])}
      onChange={fetchInitialData(store)}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={rightContainerConnector(connect)}>
        <IndexRedirect to="schemas" />
        <Route path="schemas" component={landingConnector(connect)} />
        <Route path="add" component={addConnector(connect)} />
        <Route
          path=":remoteSchemaName/details"
          component={viewConnector(connect)}
        />
        <Route
          path=":remoteSchemaName/modify"
          component={editConnector(connect)}
        />
      </Route>
    </Route>
  );
};

export default getRemoteSchemaRouter;
export { appPrefix };
