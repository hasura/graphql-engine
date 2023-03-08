import React from 'react';

import { Route, IndexRedirect } from 'react-router';
import { RightContainer } from '../../Common/Layout/RightContainer';
import {
  remoteSchemaPageConnector,
  landingConnector,
  addConnector,
  editConnector,
  permissionsConnector,
} from '.';
import { FILTER_REMOTE_SCHEMAS } from './Actions';

import { appPrefix } from './constants';
import RelationshipsConnector from './Relationships';
import { RemoteSchemaDetails } from '../../../features/RemoteSchema/components/Details/RemoteSchemaDetails';

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
    dataList: state.metadata.metadataObject?.remote_schemas ?? [],
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

const getRemoteSchemaRouter = connect => {
  return (
    <Route
      path="remote-schemas"
      component={remoteSchemaPageConnector(
        connect,
        leftNavMapStateToProps,
        leftNavMapDispatchToProps
      )}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={RightContainer}>
        <IndexRedirect to="schemas" />
        <Route path="schemas" component={landingConnector(connect)} />
        <Route path="add" component={addConnector(connect)} />
        <Route
          path=":remoteSchemaName/details"
          component={RemoteSchemaDetails}
        />
        <Route
          path=":remoteSchemaName/modify"
          component={editConnector(connect)}
        />
        <Route
          path=":remoteSchemaName/permissions"
          component={permissionsConnector}
        />
        <Route
          path=":remoteSchemaName/relationships"
          component={RelationshipsConnector}
        />
      </Route>
    </Route>
  );
};

export default getRemoteSchemaRouter;
export { appPrefix };
