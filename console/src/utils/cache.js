/* eslint-disable */
let remoteSchemaCache = {};

export const cacheRemoteSchema = (schemaName, graphqlSchema) => {
  remoteSchemaCache[schemaName] = graphqlSchema;
};

export const clearRemoteSchemaCache = () => {
  remoteSchemaCache = {};
};

export { remoteSchemaCache };
