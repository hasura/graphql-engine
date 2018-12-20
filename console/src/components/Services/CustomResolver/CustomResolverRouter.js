import React from 'react';

import { Route, IndexRedirect, Link } from 'react-router';
import { layoutConnector, rightBar } from '../Layout';
import globals from '../../../Globals';
import {
  landingCustomResolverGen,
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

const listItem = (dataList, styles, currentLocation, currentResolver) => {
  if (dataList.length === 0) {
    return (
      <li
        className={styles.noTables}
        data-test="remote-schema-sidebar-no-schemas"
      >
        <i>No remote schemas available</i>
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
      <li
        className={activeTableClass}
        key={i}
        data-test={`remote-schema-sidebar-links-${i + 1}`}
      >
        <Link
          to={appPrefix + '/manage/' + d.name + '/details'}
          data-test={d.name}
        >
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
    migrationMode: state.main.migrationMode ? state.main.migrationMode : false,
    listItemTemplate: listItem,
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
  const migrationRedirects = (nextState, replaceState, cb) => {
    const state = store.getState();
    if (!state.main.migrationMode) {
      replaceState(globals.urlPrefix + appPrefix + '/manage');
      cb();
    }
    cb();
  };
  return (
    <Route
      path="remote-schemas"
      component={layoutConnector(
        connect,
        leftNavMapStateToProps,
        leftNavMapDispatchToProps
      )}
      onEnter={composeOnEnterHooks([fetchInitialData(store)])}
      onChange={fetchInitialData(store)}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={rightBar(connect)}>
        <IndexRedirect to="schemas" />
        <Route path="schemas" component={landingCustomResolverGen(connect)} />
        <Route
          path="add"
          component={addConnector(connect)}
          onEnter={composeOnEnterHooks([migrationRedirects])}
        />
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
