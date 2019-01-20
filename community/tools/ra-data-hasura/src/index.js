import {
  bulkQuery,
  selectQuery,
  countQuery,
  insertQuery,
  updateQuery,
  deleteQuery
} from './queries';

const cloneQuery = (query) => {
  return JSON.parse(JSON.stringify(query));
};

export default (serverEndpoint, headers) => {
  const convertDataRequestToHTTP = (type, resource, params) => {
    const options = {};
    let finalQuery = {};

    switch (type) {
      case 'GET_LIST':
        // select multiple
        const finalSelectQuery = cloneQuery(selectQuery);
        const finalCountQuery = cloneQuery(countQuery);

        finalSelectQuery.args.table = resource;
        finalSelectQuery.args.limit = params.pagination.perPage;
        finalSelectQuery.args.offset = (params.pagination.page * params.pagination.perPage) - params.pagination.perPage;
        finalSelectQuery.args.where = params.filter;
        finalSelectQuery.args.order_by = {column: params.sort.field, type: params.sort.order.toLowerCase()};
        finalCountQuery.args.table = resource;
        finalQuery = cloneQuery(bulkQuery);
        finalQuery.args.push(finalSelectQuery);
        finalQuery.args.push(finalCountQuery);
        break;
      case 'GET_ONE':
        // select one
        finalQuery = cloneQuery(selectQuery);
        finalQuery.args.table = resource;
        finalQuery.args.where = { id: { '$eq': params.id } };
        break;
      case 'CREATE':
        // create one
        const createFields = Object.keys(params.data);

        finalQuery = cloneQuery(insertQuery);
        finalQuery.args.table = resource;
        finalQuery.args.objects.push(params.data);
        // id is mandatory
        createFields.push('id');
        finalQuery.args.returning = createFields;
        break;
      case 'UPDATE':
        // update one
        const updateFields = Object.keys(params.data);

        finalQuery = cloneQuery(updateQuery);
        finalQuery.args.table = resource;
        finalQuery.args['$set'] = params.data;
        finalQuery.args.where = { id: { '$eq': params.id }};
        // id is mandatory
        updateFields.push('id');
        finalQuery.args.returning = updateFields;
        break;
      case 'UPDATE_MANY':
        // update multiple ids with given data
        const updateManyFields = Object.keys(params.data);

        finalQuery = cloneQuery(updateQuery);
        finalQuery.args.table = resource;
        finalQuery.args['$set'] = params.data;
        finalQuery.args.where = { 'id': { '$in': params.ids } };
        // id is mandatory
        updateManyFields.push('id');
        finalQuery.args.returning = updateManyFields;
        break;
      case 'DELETE':
        // delete one
        const deleteFields = Object.keys(params.previousData);

        finalQuery = cloneQuery(deleteQuery);
        finalQuery.args.table = resource;
        finalQuery.args.where = { id: { '$eq': params.id }};
        // id is mandatory
        deleteFields.push('id');
        finalQuery.args.returning = deleteFields;
        break;
      case 'DELETE_MANY':
        // delete multiple
        finalQuery = cloneQuery(deleteQuery);
        finalQuery.args.table = resource;
        finalQuery.args.where = { 'id': { '$in': params.ids } };
        // id is mandatory
        finalQuery.args.returning = ['id'];
        break;
      case 'GET_MANY':
        // select multiple within where clause
        finalQuery = cloneQuery(selectQuery);
        finalQuery.args.table = resource;
        finalQuery.args.where = { 'id': { '$in': params.ids } };
        break;
      case 'GET_MANY_REFERENCE':
        // select multiple with relations
        const finalManyQuery = cloneQuery(selectQuery);
        const finalManyCountQuery = cloneQuery(countQuery);

        finalManyQuery.args.table = resource;
        finalManyQuery.args.limit = params.pagination.perPage;
        finalManyQuery.args.offset = (params.pagination.page * params.pagination.perPage) - params.pagination.perPage;
        finalManyQuery.args.where = { [params.target]: params.id };
        finalManyQuery.args.order_by = {column: params.sort.field, type: params.sort.order.toLowerCase()};
        finalManyCountQuery.args.table = resource;
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
