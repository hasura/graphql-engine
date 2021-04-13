/* eslint-disable no-underscore-dangle */
import { Table } from '../../../dataSources/types';
import { TableEntry } from '../../../metadata/types';
import { PostgresTable } from '../../../dataSources/services/postgresql/types';
import { dataSource } from '../../../dataSources';
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

type MSSqlTable = {
  columns: Array<{
    column_name: string;
    data_type: string;
    data_type_name: string;
    is_nullable: 'YES' | 'NO';
    ordinal_position: number;
    table_name: string;
    table_schema: string;
  }>;
  comment: string;
  table_name: string;
  table_schema: string;
  table_type: 'TABLE' | 'VIEW';
};

type MSSqlFk = {
  table_name: string;
  table_schema: string;
  constraint_name: string;
  on_delete: string;
  on_update: string;
  ref_table: string;
  ref_table_schema: string;
  column_mapping: Array<{ column: string; referenced_column: string }>;
};

export const mergeDataMssql = (
  data: Array<{ result: string[] }>,
  metadataTables: TableEntry[]
): Table[] => {
  const result: Table[] = [];
  const tables: MSSqlTable[] = [];
  let fkRelations: MSSqlFk[] = [];
  data[0].result.slice(1).forEach(row => {
    try {
      tables.push({
        table_schema: row[0],
        table_name: row[1],
        table_type: row[2] as MSSqlTable['table_type'],
        comment: row[3],
        columns: JSON.parse(row[4]),
      });
    } catch (err) {
      console.log(err);
    }
  });

  try {
    fkRelations = JSON.parse(data[1].result.slice(1).join('')) as MSSqlFk[];
    // eslint-disable-next-line no-empty
  } catch {}

  const trackedFkData = fkRelations
    .map(fk => ({
      ...fk,
      is_table_tracked: !!metadataTables.some(
        t =>
          t.table.name === fk.table_name && t.table.schema === fk.table_schema
      ),
      is_ref_table_tracked: !!metadataTables.some(
        t =>
          t.table.name === fk.ref_table &&
          t.table.schema === fk.ref_table_schema
      ),
    }))
    .map(fk => {
      const mapping: Record<string, string> = {};
      fk.column_mapping.forEach(cols => {
        mapping[cols.column] = cols.referenced_column;
      });
      return {
        ...fk,
        column_mapping: mapping,
        ref_table_table_schema: fk.ref_table_schema,
      };
    });

  tables.forEach(table => {
    const metadataTable = metadataTables?.find(
      t =>
        t.table.schema === table.table_schema &&
        t.table.name === table.table_name
    );

    const fkConstraints = trackedFkData.filter(
      (fk: FixMe) =>
        fk.table_schema === table.table_schema &&
        fk.table_name === table.table_name
    );

    const refFkConstraints = trackedFkData.filter(
      (fk: FixMe) =>
        fk.ref_table_schema === table.table_schema &&
        fk.ref_table === table.table_name &&
        fk.is_ref_table_tracked
    );

    const relationships = [] as Table['relationships'];
    metadataTable?.array_relationships?.forEach(rel => {
      relationships.push({
        rel_def: rel.using,
        rel_name: rel.name,
        table_name: table.table_name,
        table_schema: table.table_schema,
        rel_type: 'array',
      });
    });

    metadataTable?.object_relationships?.forEach(rel => {
      relationships.push({
        rel_def: rel.using,
        rel_name: rel.name,
        table_name: table.table_name,
        table_schema: table.table_schema,
        rel_type: 'object',
      });
    });

    const rolePermMap = permKeys.reduce((rpm: Record<string, any>, key) => {
      if (metadataTable) {
        metadataTable[key]?.forEach(
          (perm: { role: string; permission: Record<string, any> }) => {
            rpm[perm.role] = {
              permissions: {
                ...(rpm[perm.role] && rpm[perm.role].permissions),
                [keyToPermission[key]]: perm.permission,
              },
            };
          }
        );
      }
      return rpm;
    }, {});

    const permissions: Table['permissions'] = Object.keys(rolePermMap).map(
      role => ({
        role_name: role,
        permissions: rolePermMap[role].permissions,
        table_name: table.table_name,
        table_schema: table.table_schema,
      })
    );

    const mergedInfo = {
      table_schema: table.table_schema,
      table_name: table.table_name,
      table_type: table.table_type,
      is_table_tracked: metadataTables.some(
        t =>
          t.table.name === table.table_name &&
          t.table.schema === table.table_schema
      ),
      columns: table.columns,
      comment: '',
      triggers: [],
      primary_key: null,
      relationships,
      permissions,
      unique_constraints: [],
      check_constraints: [],
      foreign_key_constraints: fkConstraints,
      opp_foreign_key_constraints: refFkConstraints,
      view_info: null,
      remote_relationships: [],
      is_enum: false,
      configuration: undefined,
      computed_fields: [],
    };
    result.push(mergedInfo);
  });
  return result;
};

export const mergeLoadSchemaDataPostgres = (
  data: Array<{ result: string[] }>,
  metadataTables: TableEntry[]
): Table[] => {
  const tableList = JSON.parse(data[0].result[1]) as PostgresTable[];
  const fkList = JSON.parse(data[1].result[1]) as Omit<
    Table['foreign_key_constraints'][0],
    'is_table_tracked' | 'is_ref_table_tracked'
  >[];
  const primaryKeys = JSON.parse(data[2].result[1]) as Table['primary_key'][];
  const uniqueKeys = JSON.parse(data[3].result[1]) as any;
  const checkConstraints = dataSource?.checkConstraintsSql
    ? (JSON.parse(data[4].result[1]) as Table['check_constraints'])
    : ([] as Table['check_constraints']);
  const _mergedTableData: Table[] = [];

  const trackedFkData = fkList.map(fk => ({
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

  tableList.forEach(infoSchemaTableInfo => {
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
        comment: field.comment || '',
        computed_field_name: field.name,
        name: field.name,
        table_name: tableName,
        table_schema: tableSchema,
        definition: field.definition as Table['computed_fields'][0]['definition'],
      }));

      metadataTable?.array_relationships?.forEach(rel => {
        relationships.push({
          rel_def: rel.using,
          rel_name: rel.name,
          table_name: tableName,
          table_schema: tableSchema,
          rel_type: 'array',
        });
      });

      metadataTable?.object_relationships?.forEach(rel => {
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
