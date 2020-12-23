/* eslint-disable no-underscore-dangle */
import { Table } from '../../../dataSources/types';
import { TableEntry } from '../../../metadata/types';
import { PostgresTable } from '../../../dataSources/services/postgresql/types';
import { FixMe } from '../../../types';

// TODO — each service should export `mergeLoadSchemaData` — results should be "merged" to Table object

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
  infoSchemaTableData: PostgresTable[],
  fkData: Omit<
    Table['foreign_key_constraints'][0],
    'is_table_tracked' | 'is_ref_table_tracked'
  >[],
  metadataTables: TableEntry[],
  primaryKeys: Table['primary_key'][],
  uniqueKeys: FixMe,
  checkConstraints: Table['check_constraints']
): Table[] => {
  const _mergedTableData: Table[] = [];

  const trackedFkData = fkData.map(fk => ({
    ...fk,
    is_table_tracked: !!metadataTables.some(
      t => t.table.name === fk.table_name && t.table.schema === fk.table_schema
    ),
    is_ref_table_tracked: !!metadataTables.some(
      t =>
        t.table.name === fk.ref_table &&
        t.table.schema === fk.ref_table_table_schema
    ),
  }));

  infoSchemaTableData.forEach(infoSchemaTableInfo => {
    const tableSchema = infoSchemaTableInfo.table_schema;
    const tableName = infoSchemaTableInfo.table_name;
    const metadataTable = metadataTables?.find(
      t => t.table.schema === tableSchema && t.table.name === tableName
    );

    const columns = infoSchemaTableInfo.columns;
    const comment = infoSchemaTableInfo.comment;
    const tableType = infoSchemaTableInfo.table_type;
    const triggers = infoSchemaTableInfo.triggers;
    const viewInfo = infoSchemaTableInfo.view_info;

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
    let fkConstraints: Table['foreign_key_constraints'] = [];
    let refFkConstraints: Table['foreign_key_constraints'] = [];
    let remoteRelationships: Table['remote_relationships'] = [];
    let isEnum = false;
    let configuration = {};
    let computed_fields: Table['computed_fields'] = [];
    const relationships: Table['relationships'] = [];

    if (metadataTable) {
      isEnum = metadataTable?.is_enum || false;
      configuration = metadataTable?.configuration || {};

      fkConstraints = trackedFkData.filter(
        (fk: any) =>
          fk.table_schema === tableSchema && fk.table_name === tableName
      );

      refFkConstraints = trackedFkData.filter(
        (fk: any) =>
          fk.ref_table_table_schema === tableSchema &&
          fk.ref_table === tableName &&
          fk.is_ref_table_tracked
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
