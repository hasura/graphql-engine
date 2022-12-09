import moment from 'moment';

export const createOptionEntry = (value, label, description, key) => {
  return {
    label,
    value,
    description,
    key,
  };
};

export const getFetchQuery = ({ where, limit, offset, orderBy }) => {
  const query = {
    type: 'select',
    args: {
      table: {
        name: 'hdb_query_collection',
        schema: 'hdb_catalog',
      },
      columns: ['collection_defn'],
      where: {
        ...where,
      },
      limit,
      offset,
    },
  };
  if (orderBy) {
    query.order_by = orderBy;
  }
  return query;
};

export const deleteOperationFromAllowListQuery = ({
  operationName,
  collectionName,
}) => {
  const query = {
    type: 'drop_query_from_collection',
    args: {
      collection_name: collectionName,
      query_name: operationName,
    },
  };
  return query;
};

export const getBulkQuery = query => {
  const q = {
    type: 'bulk',
    args: query,
  };
  return q;
};

export const deleteOperationsFromAllowListQuery = ({
  operationNames,
  collectionName,
}) => {
  const allQueries = operationNames.map(o => {
    const query = {
      type: 'drop_query_from_collection',
      args: {
        collection_name: collectionName,
        query_name: o,
      },
    };
    return query;
  });
  return getBulkQuery(allQueries);
};

export const checkObjectValidity = result => {
  if (result && typeof result === 'object') {
    return true;
  }
  return false;
};

const transformTimeType = d => {
  return moment(d).fromNow();
};

export const transformedVals = {
  last_seen: transformTimeType,
};

export const getAllowList = (metadata, collectionName) => {
  const query_collections = metadata?.query_collections || [];
  return query_collections.find(i => i.name === collectionName);
};
