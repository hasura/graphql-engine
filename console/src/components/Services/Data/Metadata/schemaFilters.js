export const filterSchema = (schemas, inconsistentObject) => {
  if (inconsistentObject.type === 'table') {
    return schemas.filter(s => s.table_name !== inconsistentObject.definition);
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
};
