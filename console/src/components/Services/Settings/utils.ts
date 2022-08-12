export const permissionTypes = [
  'select_permission',
  'update_permission',
  'insert_permission',
  'delete_permission',
];

export type MetadataObject = {
  name: string;
  function_name: string;
  type:
    | 'table'
    | 'array_relation'
    | 'object_relation'
    | 'function'
    | 'event_trigger'
    | 'remote_schema'
    | 'action';
  definition: {
    name: string;
    role: string;
    table: {
      name: string;
    };
    configuration: {
      name: string;
    };
  };
  table_name: string;
  relationships: Array<{ rel_name: string }>;
  permissions: Array<{ role_name: string }>;
};

export const getTableNameFromDef = (
  def:
    | {
        name: string;
      }
    | string
) => {
  if (typeof def === 'string') {
    return def;
  }
  return def.name;
};

const filterInconsistentMetadataObject = (
  objects: Record<string, any>[],
  inconsistentObject: Record<string, any>,
  type: string
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
              relationships: schema.relationships.filter((r: any) => {
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
              permissions: schema.permissions.filter((p: any) => {
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
    case 'actions':
      const actions = objects;

      if (inconsistentObject.type === 'action') {
        return actions.filter(
          t => t.name !== inconsistentObject.definition.name
        );
      }

      return actions;
    default:
      return objects;
  }
};

export const filterInconsistentMetadataObjects = (
  metadataObjects: any[],
  inconsistentObjects: Record<string, any>[],
  type: string
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
