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

export default function (serverEndpoint, headers, configs) {
    // console.log(arguments);
    const convertDataRequestToHTTP = (type, resource, params) => {
        const options = {};
        let finalQuery = {};

        let schema;
        let tableName;

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
        // console.log(type);

        let primaryKey = configs[resource] && configs[resource].primaryKey ? configs[resource].primaryKey : "id";

        switch (type) {
            case 'GET_LIST':
                // select multiple
                const finalSelectQuery = cloneQuery(selectQuery);
                const finalCountQuery = cloneQuery(countQuery);

                finalSelectQuery.args.table = {'name': tableName, 'schema': schema};
                finalSelectQuery.args.limit = params.pagination.perPage;
                finalSelectQuery.args.offset = (params.pagination.page * params.pagination.perPage) - params.pagination.perPage;
                finalSelectQuery.args.where = params.filter;
                // error is here / so params is empty?
                finalSelectQuery.args.order_by = {column: params.sort.field, type: params.sort.order.toLowerCase()};
                // finalSelectQuery.args.order_by = {column: primaryKey, type: params.sort.order.toLowerCase()};
                console.log(params);
                finalCountQuery.args.table = {'name': tableName, 'schema': schema};
                finalSelectQuery.where = {};
                finalCountQuery.where = {};
                finalQuery = cloneQuery(bulkQuery);
                // console.log(finalCountQuery.where = {});
                finalQuery.args.push(finalSelectQuery);
                finalQuery.args.push(finalCountQuery);
                break;
            case 'GET_ONE':
                // select one
                finalQuery = cloneQuery(selectQuery);
                finalQuery.args.table = {'name': tableName, 'schema': schema};
                finalQuery.args.where = {[primaryKey]: {'$eq': params.id}};
                break;
            case 'CREATE':
                // create one
                const createFields = Object.keys(params.data);

                finalQuery = cloneQuery(insertQuery);
                finalQuery.args.table = {'name': tableName, 'schema': schema};
                finalQuery.args.objects.push(params.data);
                // id is mandatory
                createFields.push('id');
                finalQuery.args.returning = createFields;
                break;
            case 'UPDATE':
                // update one
                const updateFields = Object.keys(params.data);

                finalQuery = cloneQuery(updateQuery);
                finalQuery.args.table = {'name': tableName, 'schema': schema};
                finalQuery.args['$set'] = params.data;
                finalQuery.args.where = {id: {'$eq': params.id}};
                // id is mandatory
                updateFields.push('id');
                finalQuery.args.returning = updateFields;
                break;
            case 'UPDATE_MANY':
                // update multiple ids with given data
                const updateManyFields = Object.keys(params.data);

                finalQuery = cloneQuery(updateQuery);
                finalQuery.args.table = {'name': tableName, 'schema': schema};
                finalQuery.args['$set'] = params.data;
                finalQuery.args.where = {'id': {'$in': params.ids}};
                // id is mandatory
                updateManyFields.push('id');
                finalQuery.args.returning = updateManyFields;
                break;
            case 'DELETE':
                // delete one
                const deleteFields = Object.keys(params.previousData);

                finalQuery = cloneQuery(deleteQuery);
                finalQuery.args.table = {'name': tableName, 'schema': schema};
                finalQuery.args.where = {id: {'$eq': params.id}};
                // id is mandatory
                deleteFields.push('id');
                finalQuery.args.returning = deleteFields;
                break;
            case 'DELETE_MANY':
                // delete multiple
                finalQuery = cloneQuery(deleteQuery);
                finalQuery.args.table = {'name': tableName, 'schema': schema};
                finalQuery.args.where = {'id': {'$in': params.ids}};
                // id is mandatory
                finalQuery.args.returning = ['id'];
                break;
            case 'GET_MANY':
                // select multiple within where clause
                finalQuery = cloneQuery(selectQuery);
                finalQuery.args.table = {'name': tableName, 'schema': schema};
                finalQuery.args.where = {'id': {'$in': params.ids}};
                break;
            case 'GET_MANY_REFERENCE':
                // select multiple with relations
                const finalManyQuery = cloneQuery(selectQuery);
                const finalManyCountQuery = cloneQuery(countQuery);

                finalManyQuery.args.table = {'name': tableName, 'schema': schema};
                finalManyQuery.args.limit = params.pagination.perPage;
                finalManyQuery.args.offset = (params.pagination.page * params.pagination.perPage) - params.pagination.perPage;
                finalManyQuery.args.where = {[params.target]: params.id};
                finalManyQuery.args.order_by = {column: params.sort.field, type: params.sort.order.toLowerCase()};
                finalManyCountQuery.args.table = {'name': tableName, 'schema': schema};
                finalQuery = cloneQuery(bulkQuery);
                finalQuery.args.push(finalManyQuery);
                finalQuery.args.push(finalManyCountQuery);
                break;
            default:
                throw new Error(`Unsupported type ${type}`);
        }

        options.body = JSON.stringify(finalQuery);
        return {options};
    };

    function convertHTTPResponse(response, type, resource, params) {
        // handle errors and throw with the message
        if ('error' in response || 'code' in response) {
            throw new Error(JSON.stringify(response));
        }

        // console.log(arguments);

// this console not show
        console.log("configs: " + JSON.stringify(configs, null, 2));
        switch (type) {
            case 'GET_LIST':
                // console.log(resource);
                let primaryKey = configs[resource] && configs[resource].primaryKey ? configs[resource].primaryKey : "id";
                console.log(`primary key for ${resource} = ${primaryKey}`);
                // console.log(response);
                return {
                    data:response[0].map(res => ({ ...res, id: res[primaryKey]})),
                    total: response[1]['count']
                };
            case 'GET_ONE':
                return {
                    data: { ...response[0], id: response[0][primaryKey]}
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
                return {data: response};
        }
    }

    return (type, resource, params) => {
        const {options} = convertDataRequestToHTTP(
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

