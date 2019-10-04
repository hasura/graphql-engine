export const permissionTypes = [
  'select_permission',
  'update_permission',
  'insert_permission',
  'delete_permission',
];

export const getTableNameFromDef = def => {
  if (def.constructor.name === 'Object') {
    return def.name;
  }
  return def;
};

const filterInconsistentMetadataObject = (
  objects,
  inconsistentObject,
  type
) => {
  switch (type) {
    case 'tables':
      const schemas = objects;

      if (inconsistentObject.type === 'table') {
        const tableName = getTableNameFromDef(inconsistentObject.definition);
        return schemas.filter(s => s.table_name !== tableName);
      }

      if (
        inconsistentObject.type === 'array_relation' ||
        inconsistentObject.type === 'object_relation'
      ) {
        const { table } = inconsistentObject.definition;
        return schemas.map(schema => {
          if (schema.table_name === getTableNameFromDef(table)) {
            return {
              ...schema,
              relationships: schema.relationships.filter(r => {
                return r.rel_name !== inconsistentObject.definition.name;
              }),
            };
          }
          return schema;
        });
      }

      if (permissionTypes.includes(inconsistentObject.type)) {
        const { table } = inconsistentObject.definition;
        return schemas.map(schema => {
          if (schema.table_name === getTableNameFromDef(table)) {
            return {
              ...schema,
              permissions: schema.permissions.filter(p => {
                return p.role_name !== inconsistentObject.definition.role;
              }),
            };
          }
          return schema;
        });
      }

      return schemas;
    case 'functions':
      const functions = objects;

      if (inconsistentObject.type === 'function') {
        return functions.filter(
          f =>
            f.function_name !==
            getTableNameFromDef(inconsistentObject.definition)
        );
      }

      return functions;
    case 'events':
      const triggers = objects;

      if (inconsistentObject.type === 'event_trigger') {
        return triggers.filter(
          t => t.name !== inconsistentObject.definition.configuration.name
        );
      }

      return triggers;
    case 'remote_schemas':
      const remoteSchemas = objects;

      if (inconsistentObject.type === 'remote_schema') {
        return remoteSchemas.filter(
          t => t.name !== inconsistentObject.definition.name
        );
      }

      return remoteSchemas;
    default:
      return objects;
  }
};

export const filterInconsistentMetadataObjects = (
  metadataObjects,
  inconsistentObjects,
  type
) => {
  let filteredMetadataObjects = JSON.parse(JSON.stringify(metadataObjects));

  inconsistentObjects.forEach(object => {
    filteredMetadataObjects = filterInconsistentMetadataObject(
      filteredMetadataObjects,
      object,
      type
    );
  });

  return filteredMetadataObjects;
};
