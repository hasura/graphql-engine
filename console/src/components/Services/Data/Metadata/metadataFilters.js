export const filterSchema = (metadata, inconsistentObject, type) => {
  if (type === 'tables') {
    const schemas = metadata;
    if (inconsistentObject.type === 'table') {
      return schemas.filter(
        s => s.table_name !== inconsistentObject.definition
      );
    }
    if (
      inconsistentObject.type === 'array_relation' ||
      inconsistentObject.type === 'object_relation'
    ) {
      const { table } = inconsistentObject.definition;
      return schemas.map(schema => {
        if (schema.table_name === table) {
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
    if (
      inconsistentObject.type === 'select_permission' ||
      inconsistentObject.type === 'update_permission' ||
      inconsistentObject.type === 'delete_permission'
    ) {
      const { table } = inconsistentObject.definition;
      return schemas.map(schema => {
        if (schema.table_name === table) {
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
        f => f.function_name !== inconsistentObject.definition
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
