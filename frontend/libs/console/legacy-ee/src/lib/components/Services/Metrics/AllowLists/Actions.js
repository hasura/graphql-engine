import endpoints from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';

export const createCollectionIfNotExist =
  collectionName => (dispatch, getState) => {
    const { dataHeaders } = getState().tables;

    const createQueryCollection = {
      type: 'create_query_collection',
      args: {
        name: collectionName,
        comment: 'Managed by pro hasura',
        definition: {
          queries: [],
        },
      },
    };

    const addCollectionToAllowList = {
      type: 'add_collection_to_allowlist',
      args: {
        collection: collectionName,
      },
    };

    const bulkQuery = {
      type: 'bulk',
      args: [createQueryCollection, addCollectionToAllowList],
    };

    const options = {
      method: 'POST',
      headers: {
        ...dataHeaders,
      },
      body: JSON.stringify(bulkQuery),
    };

    return dispatch(requestAction(endpoints.queryV1, options));
  };

export const deleteCollectionIfExist =
  collectionName => (dispatch, getState) => {
    const { dataHeaders } = getState().tables;

    const dropQueryCollection = {
      type: 'drop_query_collection',
      args: {
        collection: collectionName,
        cascade: false,
      },
    };

    const dropCollectionFromAllowList = {
      type: 'drop_collection_from_allowlist',
      args: {
        collection: collectionName,
      },
    };

    const bulkQuery = {
      type: 'bulk',
      args: [dropCollectionFromAllowList, dropQueryCollection],
    };

    const options = {
      method: 'POST',
      headers: {
        ...dataHeaders,
      },
      body: JSON.stringify(bulkQuery),
    };

    return dispatch(requestAction(endpoints.queryV1, options));
  };

export const makeQuery = query => (dispatch, getState) => {
  const { dataHeaders } = getState().tables;

  const options = {
    method: 'POST',
    headers: {
      ...dataHeaders,
    },
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(endpoints.queryV1, options));
};

export const applyAllowList = exportList => (dispatch, getState) => {
  const { dataHeaders } = getState().tables;

  const query = {
    type: 'bulk',
    args: exportList,
  };

  const options = {
    method: 'POST',
    headers: {
      ...dataHeaders,
    },
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(endpoints.queryV1, options));
};
