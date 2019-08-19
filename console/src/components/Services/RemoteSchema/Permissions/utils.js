export const generateCreatePermQuery = (state, remoteSchemaName) => {
  const { role, allowedTypes } = state;

  const payload = {
    type: 'add_remote_schema_permissions',
    args: {
      remote_schema: remoteSchemaName,
      role,
      definition: {},
    },
  };

  Object.keys(allowedTypes).forEach(allowedType => {
    payload.args.definition[allowedType] = Object.keys(
      allowedTypes[allowedType]
    ).filter(fieldName => allowedTypes[allowedType][fieldName].isChecked);
  });

  return payload;
};
