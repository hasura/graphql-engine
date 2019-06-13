import React from 'react';

import { Route, IndexRedirect } from 'react-router';
import { rightContainerConnector } from '../../Common/Layout';
import globals from '../../../Globals';
import {
  customResolverPageConnector,
  landingConnector,
  addConnector,
  editConnector,
  viewConnector,
} from '.';
import { fetchResolvers, FILTER_RESOLVER } from './customActions';

// Objective is to render list of custom resolvers on the
// left nav bar.
// Custom resolvers list is fetched from hdb_catalog/custom_resolver
// Whenever any operation happens like add resolver/delete resolver, this state should update automatically.

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
      type: FILTER_RESOLVER,
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
    dataList: [...state.customResolverData.listData.resolvers],
    isError: state.customResolverData.listData.isError,
    isRequesting: state.customResolverData.listData.isRequesting,
    filtered: [...state.customResolverData.listData.filtered],
    searchQuery: state.customResolverData.listData.searchQuery,
    viewResolver: state.customResolverData.listData.viewResolver,
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
    const dataList = currState.customResolverData.listData.resolvers;
    if (dataList.length) {
      cb();
      return;
    }
    */

    Promise.all([dispatch(fetchResolvers())]).then(
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

const getCustomResolverRouter = (connect, store, composeOnEnterHooks) => {
  return (
    <Route
      path="remote-schemas"
      component={customResolverPageConnector(
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
          path=":resolverName/details"
          component={viewConnector(connect)}
        />
        <Route path=":resolverName/modify" component={editConnector(connect)} />
      </Route>
    </Route>
  );
};

export default getCustomResolverRouter;
export { appPrefix };
