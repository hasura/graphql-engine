import React from 'react';

import { Route, IndexRedirect, Link } from 'react-router';

import { layoutConnector, rightBar } from '../Layout';

import globals from '../../../Globals';

import { landingCustomResolverGen, addConnector, editConnector } from '.';

import { fetchResolvers, FILTER_RESOLVER } from './customActions';

// Objective is to render list of custom resolvers on the
// left nav bar.
// Custom resolvers list is fetched from hdb_catalog/custom_resolver
// Whenever any operation happens like add resolver/delete resolver, this state should update automatically.

const appPrefix = '/custom-resolver';

const listItem = (dataList, styles, currentLocation, currentResolver) => {
  if (dataList.length === 0) {
    return (
      <li className={styles.noTables}>
        <i>No resolvers available</i>
      </li>
    );
  }
  return dataList.map((d, i) => {
    let activeTableClass = '';
    if (
      d.name === currentResolver &&
      currentLocation.pathname.indexOf(currentResolver) !== -1
    ) {
      activeTableClass = styles.activeTable;
    }
    return (
      <li className={activeTableClass} key={i}>
        <Link to={appPrefix + '/manage/' + d.name + '/edit'} data-test={d.name}>
          <i className={styles.tableIcon + ' fa fa-table'} aria-hidden="true" />
          {d.name}
        </Link>
      </li>
    );
  });
};

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
    listItemTemplate: listItem,
  };
};

const leftNavMapDispatchToProps = dispatch => {
  return {
    filterItem: filterItem(dispatch),
  };
};

const fetchInitialData = ({ dispatch, getState }) => {
  return (nextState, replaceState, cb) => {
    const currState = getState();
    const dataList = currState.customResolverData.listData.resolvers;
    if (dataList.length) {
      cb();
      return;
    }
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
      path="custom-resolver"
      component={layoutConnector(
        connect,
        leftNavMapStateToProps,
        leftNavMapDispatchToProps
      )}
      onEnter={composeOnEnterHooks([fetchInitialData(store)])}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={rightBar(connect)}>
        <IndexRedirect to="resolvers" />
        <Route path="resolvers" component={landingCustomResolverGen(connect)} />
        <Route path="add" component={addConnector(connect)} />
        <Route path=":resolverName/edit" component={editConnector(connect)} />
      </Route>
    </Route>
  );
};

export default getCustomResolverRouter;
