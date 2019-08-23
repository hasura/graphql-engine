import {
  bulkQuery,
  selectQuery,
  countQuery,
  insertQuery,
  updateQuery,
  deleteQuery
} from './queries';

const DEFAULT_PRIMARY_KEY = 'id';

const cloneQuery = (query) => {
  return JSON.parse(JSON.stringify(query));
};

export default (serverEndpoint, headers, config) => {

  const getTableSchema = (resource) => {
    let tableName;
    let schema;

    // parse schema in resource
    if (resource && resource.split('.').length === 1) {
      schema = 'public';
      tableName = resource;
    } else if (resource && resource.split('.').length === 2) {
      const resourceSplit = resource.split('.');

      schema = resourceSplit[0];
      tableName = resourceSplit[1];
    } else {
      throw new Error(JSON.stringify({'error': 'Invalid table/schema resource'}));
    }
    return { schema, tableName };
  };

  const getPrimaryKey = (resource) => {
    let primaryKey = DEFAULT_PRIMARY_KEY;

    if (config && config['primaryKey'][resource]) {
      primaryKey = config['primaryKey'][resource];
    }
    return primaryKey;
  };

  const addFilters = (where, filter) => {
    if (!filter) return where;

    const filterKeys = Object.keys(filter);
    if (filterKeys.length === 0) return where;

    const whereCopy = Object.assign(where)
    filterKeys.forEach((key) => {
      whereCopy[key] = filter[key];
    });
    return whereCopy;
  };

  const convertDataRequestToHTTP = (type, resource, params) => {
    const options = {};
    let finalQuery = {};

    const tableSchema = getTableSchema(resource);

    let { schema, tableName } = tableSchema;

    const primaryKey = getPrimaryKey(resource);

    switch (type) {
      case 'GET_LIST':
        // select multiple
        const finalSelectQuery = cloneQuery(selectQuery);
        const finalCountQuery = cloneQuery(countQuery);

        finalSelectQuery.args.table = {'name': tableName, 'schema': schema};
        finalSelectQuery.args.limit = params.pagination.perPage;
        finalSelectQuery.args.offset = (params.pagination.page * params.pagination.perPage) - params.pagination.perPage;
        finalSelectQuery.args.where = params.filter;
        finalSelectQuery.args.order_by = {column: params.sort.field || primaryKey, type: params.sort.order.toLowerCase()};
        finalCountQuery.args.table = {'name': tableName, 'schema': schema};;
        finalCountQuery.args.where = {};
        finalCountQuery.args.where[primaryKey] = { '$ne': null };
        finalCountQuery.args.where = addFilters(finalCountQuery.args.where, params.filter);
        finalQuery = cloneQuery(bulkQuery);
        finalQuery.args.push(finalSelectQuery);
        finalQuery.args.push(finalCountQuery);
        break;
      case 'GET_ONE':
        // select one
        finalQuery = cloneQuery(selectQuery);
        finalQuery.args.table = {'name': tableName, 'schema': schema};
        finalQuery.args.where = {};
        finalQuery.args.where[primaryKey] = { '$eq': params.id };
        break;
      case 'CREATE':
        // create one
        const createFields = Object.keys(params.data);

        finalQuery = cloneQuery(insertQuery);
        finalQuery.args.table = {'name': tableName, 'schema': schema};
        finalQuery.args.objects.push(params.data);
        createFields.push(primaryKey);
        finalQuery.args.returning = createFields;
        break;
      case 'UPDATE':
        // update one
        const updateFields = Object.keys(params.data);

        finalQuery = cloneQuery(updateQuery);
        finalQuery.args.table = {'name': tableName, 'schema': schema};
        finalQuery.args['$set'] = params.data;
        finalQuery.args.where = {};
        finalQuery.args.where[primaryKey] = { '$eq': params.id };
        updateFields.push(primaryKey);
        finalQuery.args.returning = updateFields;
        break;
      case 'UPDATE_MANY':
        // update multiple ids with given data
        const updateManyFields = Object.keys(params.data);

        finalQuery = cloneQuery(updateQuery);
        finalQuery.args.table = {'name': tableName, 'schema': schema};
        finalQuery.args['$set'] = params.data;
        finalQuery.args.where = {};
        finalQuery.args.where[primaryKey] = { '$in': params.ids };
        updateManyFields.push(primaryKey);
        finalQuery.args.returning = updateManyFields;
        break;
      case 'DELETE':
        // delete one
        const deleteFields = Object.keys(params.previousData);

        finalQuery = cloneQuery(deleteQuery);
        finalQuery.args.table = {'name': tableName, 'schema': schema};
        finalQuery.args.where = {};
        finalQuery.args.where[primaryKey] = { '$eq': params.id };
        deleteFields.push(primaryKey);
        finalQuery.args.returning = deleteFields;
        break;
      case 'DELETE_MANY':
        // delete multiple
        finalQuery = cloneQuery(deleteQuery);
        finalQuery.args.table = {'name': tableName, 'schema': schema};
        finalQuery.args.where = {};
        finalQuery.args.where[primaryKey] = { '$in': params.ids };
        finalQuery.args.returning = [primaryKey];
        break;
      case 'GET_MANY':
        // select multiple within where clause
        finalQuery = cloneQuery(selectQuery);
        finalQuery.args.table = {'name': tableName, 'schema': schema};
        finalQuery.args.where = {};
        finalQuery.args.where[primaryKey] = { '$in': params.ids };
        finalQuery.args.where = addFilters(finalQuery.args.where, params.filter);
        break;
      case 'GET_MANY_REFERENCE':
        // select multiple with relations
        const finalManyQuery = cloneQuery(selectQuery);
        const finalManyCountQuery = cloneQuery(countQuery);

        finalManyQuery.args.table = {'name': tableName, 'schema': schema};
        finalManyQuery.args.limit = params.pagination.perPage;
        finalManyQuery.args.offset = (params.pagination.page * params.pagination.perPage) - params.pagination.perPage;
        finalManyQuery.args.where = { [params.target]: params.id };
        finalManyQuery.args.where = addFilters(finalManyQuery.args.where, params.filter);
        finalManyQuery.args.order_by = {column: params.sort.field || primaryKey, type: params.sort.order.toLowerCase()};
        finalManyCountQuery.args.table = {'name': tableName, 'schema': schema};;
        finalManyCountQuery.args.where = {};
        finalManyCountQuery.args.where[primaryKey] = { '$ne': null };
        finalManyCountQuery.args.where = addFilters(finalManyQuery.args.where, params.filter);
        finalQuery = cloneQuery(bulkQuery);
        finalQuery.args.push(finalManyQuery);
        finalQuery.args.push(finalManyCountQuery);
        break;
      default:
        throw new Error(`Unsupported type ${type}`);
    };
    options.body = JSON.stringify(finalQuery);
    return { options };
  };

  const convertHTTPResponse = (response, type, resource, params) => {
    // handle errors and throw with the message
    if ('error' in response || 'code' in response) {
      throw new Error(JSON.stringify(response));
    }
    const primaryKey = getPrimaryKey(resource);

    if (primaryKey !== DEFAULT_PRIMARY_KEY) {
      if (Array.isArray(response[0])) {
        response[0].forEach((res) => {
          res[DEFAULT_PRIMARY_KEY] = res[primaryKey];
        })
      } else {
        response[0][DEFAULT_PRIMARY_KEY] = response[0][primaryKey];
      }
    }
    switch (type) {
      case 'GET_LIST':
        return {
          data: response[0],
          total: response[1]['count']
        };
      case 'GET_ONE':
        return {
          data: response[0]
        };
      case 'CREATE':
        return {
          data: response.returning[0]
        };
      case 'UPDATE':
        return {
          data: response.returning[0]
        };
      case 'UPDATE_MANY':
        const updatedIds = response.returning.map((item) => {
          return item.id;
        });

        return {
          data: updatedIds
        };
      case 'DELETE':
        return {
          data: response.returning[0]
        };
      case 'DELETE_MANY':
        const deletedIds = response.returning.map((item) => {
          return item.id;
        });

        return {
          data: deletedIds
        };
      case 'GET_MANY':
        return {
          data: response
        };
      case 'GET_MANY_REFERENCE':
        return {
          data: response[0],
          total: response[1].count
        };
      default:
        return { data: response };
    }
  };

  return (type, resource, params) => {
    const { options } = convertDataRequestToHTTP(
      type,
      resource,
      params
    );

    options.method = 'POST';
    options.headers = headers;
    return fetch(serverEndpoint + '/v1/query', options).then(function (response) {
      return response.json().then((data) => {
        return convertHTTPResponse(data, type, resource, params);
      });
    });
  };
};
