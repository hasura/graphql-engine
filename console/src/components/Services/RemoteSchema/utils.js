export const getAllRemoteSchemas = getState => {
  return getState().remoteSchemas.listData.remoteSchemas;
};

export const getRemoteSchemaName = remoteSchema => {
  return remoteSchema.name;
};

export const findRemoteSchema = (remoteSchemas, remoteSchemaName) => {
  return remoteSchemas.find(a => getRemoteSchemaName(a) === remoteSchemaName);
};

export const getRemoteSchemaPermissions = remoteSchema => {
  return remoteSchema.permissions;
};

export const findRemoteSchemaPermission = (perms, role) => {
  return perms.find(p => p.role_name === role);
};
