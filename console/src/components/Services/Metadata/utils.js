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

export const filterSchema = (metadata, inconsistentObject, type) => {
  if (type === 'tables') {
    const schemas = metadata;
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
  } else if (type === 'functions') {
    const functions = metadata;
    if (inconsistentObject.type === 'function') {
      return functions.filter(
        f =>
          f.function_name !== getTableNameFromDef(inconsistentObject.definition)
      );
    }
    return functions;
  } else if (type === 'events') {
    const triggers = metadata;
    if (inconsistentObject.type === 'event_trigger') {
      return triggers.filter(
        t => t.name !== inconsistentObject.definition.configuration.name
      );
    }
    return triggers;
  }
  return metadata;
};

export const reloadMetadataVersionSym = 'reloadMetaDataApiChange';
