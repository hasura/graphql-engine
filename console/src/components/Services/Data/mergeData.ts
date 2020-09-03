/* eslint-disable no-underscore-dangle */
import { Table } from '../../../dataSources/types';
import { TableEntry } from '../../../metadata/types';
import { PostgresTable } from '../../../dataSources/services/postgresql/types';

// TODO -- each service should export this function and results should be "merged" to Table object
// copied from utils
type PermKeys = Pick<
  TableEntry,
  | 'update_permissions'
  | 'select_permissions'
  | 'delete_permissions'
  | 'insert_permissions'
>;

const permKeys: Array<keyof PermKeys> = [
  'insert_permissions',
  'update_permissions',
  'select_permissions',
  'delete_permissions',
];

const keyToPermission = {
  insert_permissions: 'insert',
  update_permissions: 'update',
  select_permissions: 'select',
  delete_permissions: 'delete',
};

export const mergeLoadSchemaData = (
  infoSchemaTableData: PostgresTable[], // todo
  fkData: any,
  refFkData: any,
  metadataTables: TableEntry[],
  primaryKeys: Table['primary_key'][],
  uniqueKeys: any, // todo
  checkConstraints: Table['check_constraints']
): Table[] => {
  const _mergedTableData: Table[] = [];

  infoSchemaTableData.forEach(infoSchemaTableInfo => {
    const tableSchema = infoSchemaTableInfo.table_schema;
    const tableName = infoSchemaTableInfo.table_name;
    const metadataTable = metadataTables?.find(
      t => t.table.schema === tableSchema && t.table.name === tableName
    );

    const columns = infoSchemaTableInfo.columns;
    const comment = infoSchemaTableInfo.comment;
    const tableType = infoSchemaTableInfo.table_type;
    const triggers = infoSchemaTableInfo.triggers; // TODO: get from v1/query
    const viewInfo = infoSchemaTableInfo.view_info; // TODO: get from v1/query

    const keys =
      primaryKeys.find(
        key => key?.table_name === tableName && key.table_schema === tableSchema
      ) || null;

    const unique =
      uniqueKeys.filter(
        (key: any) =>
          key?.table_name === tableName && key.table_schema === tableSchema
      ) || [];

    const check =
      checkConstraints.filter(
        (key: any) =>
          key?.table_name === tableName && key.table_schema === tableSchema
      ) || [];

    const permissions: Table['permissions'] = [];
    let fkConstraints = [];
    let refFkConstraints = [];
    let remoteRelationships: any = []; // todo, update type Table
    let isEnum = false;
    let configuration = {};
    let computed_fields: Table['computed_fields'] = [];
    const relationships: Table['relationships'] = [];

    if (metadataTable) {
      isEnum = metadataTable?.is_enum || false;
      configuration = metadataTable?.configuration || {};

      fkConstraints = fkData.filter(
        (fk: any) =>
          fk.table_schema === tableSchema && fk.table_name === tableName
      );

      refFkConstraints = refFkData.filter(
        (fk: any) =>
          fk.ref_table_table_schema === tableSchema &&
          fk.ref_table === tableName
      );

      remoteRelationships = (metadataTable?.remote_relationships || []).map(
        ({ definition, name }) => ({
          remote_relationship_name: name,
          table_name: tableName,
          table_schema: tableSchema,
          definition,
        })
      );

      computed_fields = (metadataTable?.computed_fields || []).map(field => ({
        comment: field.comment,
        computed_field_name: field.name,
        name: field.name,
        table_name: tableName,
        table_schema: tableSchema,
        definition: field.definition as Table['computed_fields'][0]['definition'],
      }));

      metadataTable?.array_relationships?.map(rel => {
        relationships.push({
          rel_def: rel.using,
          rel_name: rel.name,
          table_name: tableName,
          table_schema: tableSchema,
          rel_type: 'array',
        });
      });

      metadataTable?.object_relationships?.map(rel => {
        relationships.push({
          rel_def: rel.using,
          rel_name: rel.name,
          table_name: tableName,
          table_schema: tableSchema,
          rel_type: 'object',
        });
      });

      const rolePermMap: Record<string, any> = {};

      permKeys.forEach(key => {
        console.log({ metadataTable });
        if (metadataTable) {
          metadataTable[key]?.forEach((perm: any) => {
            rolePermMap[perm.role] = {
              permissions: {
                ...(rolePermMap[perm.role] &&
                  rolePermMap[perm.role].permissions),
                [keyToPermission[key]]: perm.permission,
              },
            };
          });
        }
      });

      Object.keys(rolePermMap).forEach(role => {
        permissions.push({
          role_name: role,
          permissions: rolePermMap[role].permissions,
          table_name: tableName,
          table_schema: tableSchema,
        });
      });
    }

    const _mergedInfo = {
      table_schema: tableSchema,
      table_name: tableName,
      table_type: tableType as Table['table_type'],
      is_table_tracked: !!metadataTable,
      columns,
      comment,
      triggers,
      primary_key: keys,
      relationships,
      permissions,
      unique_constraints: unique,
      check_constraints: check,
      foreign_key_constraints: fkConstraints,
      opp_foreign_key_constraints: refFkConstraints,
      view_info: viewInfo as Table['view_info'],
      remote_relationships: remoteRelationships,
      is_enum: isEnum,
      configuration: configuration as Table['configuration'],
      computed_fields,
    };

    _mergedTableData.push(_mergedInfo);
  });

  return _mergedTableData;
};
