import { Nullable } from '../components/Common/utils/tsUtils';
import { Table, Relationship, CheckConstraint } from './types';
import { TableDefinition } from '../components/Common/utils/v1QueryUtils'; // TODO
import { isEqual } from '../components/Common/utils/jsUtils';

export type Operations = 'insert' | 'select' | 'update' | 'delete';
export const QUERY_TYPES: Operations[] = [
  'insert',
  'select',
  'update',
  'delete',
];

export const generateTableDef = (
  tableName: string,
  tableSchema: Nullable<string> = 'public',
  tableNameWithSchema: Nullable<string> = null
) => {
  if (tableNameWithSchema) {
    return {
      schema: tableNameWithSchema.split('.')[0],
      name: tableNameWithSchema.split('.')[1],
    };
  }
  return {
    schema: tableSchema || 'public',
    name: tableName,
  };
};

export const getTableDef = (table: Table) => {
  return generateTableDef(table.table_name, table.table_schema);
};

export const getQualifiedTableDef = (tableDef: TableDefinition | string) => {
  return typeof tableDef === 'string' ? generateTableDef(tableDef) : tableDef;
};

export const getTableNameWithSchema = (
  tableDef: TableDefinition,
  wrapDoubleQuotes = false
) => {
  let fullTableName;

  if (wrapDoubleQuotes) {
    fullTableName = `"${tableDef.schema}"."${tableDef.name}"`;
  } else {
    fullTableName = `${tableDef.schema}.${tableDef.name}`;
  }

  return fullTableName;
};

export const findTable = (allTables: Table[], tableDef: TableDefinition) => {
  return allTables.find(t => isEqual(getTableDef(t), tableDef));
};

export const getTrackedTables = (tables: Table[]) => {
  return tables.filter(t => t.is_table_tracked);
};

export const getUntrackedTables = (tables: Table[]) => {
  return tables.filter(t => !t.is_table_tracked);
};

export const getTableColumnNames = (table: Table) => {
  return table.columns.map(c => c.column_name);
};

export const getTableColumn = (table: Table, columnName: string) => {
  return (table.columns || []).find(
    column => column.column_name === columnName
  );
};

export const getTableRelationshipNames = (table: Table) => {
  return table.relationships.map(r => r.rel_name);
};

export function getTableRelationship(table: Table, relationshipName: string) {
  return table.relationships.find(
    relationship => relationship.rel_name === relationshipName
  );
}

export function getRelationshipRefTable(
  table: Table,
  relationship: Relationship
) {
  let refTable = null;

  const relationshipDef = relationship.rel_def;
  const relationshipType = relationship.rel_type;

  // if manual relationship
  if (relationshipDef.manual_configuration) {
    refTable = relationshipDef.manual_configuration.remote_table;
  }

  // if foreign-key based relationship
  if (relationshipDef.foreign_key_constraint_on) {
    // if array relationship
    if (relationshipType === 'array') {
      refTable = relationshipDef.foreign_key_constraint_on.table;
    }

    // if object relationship
    if (relationshipType === 'object') {
      const fkCol = relationshipDef.foreign_key_constraint_on;

      for (let i = 0; i < table.foreign_key_constraints.length; i++) {
        const fkConstraint = table.foreign_key_constraints[i];
        const fkConstraintCol = Object.keys(fkConstraint.column_mapping)[0];
        if (fkCol === fkConstraintCol) {
          refTable = generateTableDef(
            fkConstraint.ref_table,
            fkConstraint.ref_table_table_schema
          );
          break;
        }
      }
    }
  }

  if (typeof refTable === 'string') {
    refTable = generateTableDef(refTable);
  }

  return refTable;
}

export const getEnumColumnMappings = (
  allSchemas: Table[],
  tableName: string,
  tableSchema: string
) => {
  const currentTable = findTable(
    allSchemas,
    generateTableDef(tableName, tableSchema)
  );

  const relationsMap: any[] = [];
  if (!currentTable) return null;
  if (!currentTable.foreign_key_constraints.length) return null;

  currentTable.foreign_key_constraints.forEach(
    ({ ref_table, ref_table_table_schema, column_mapping }) => {
      const refTable = findTable(
        allSchemas,
        generateTableDef(ref_table, ref_table_table_schema)
      );

      if (!refTable || !refTable.is_enum) return;

      const keys = Object.keys(column_mapping);
      if (!keys.length) return;

      const columnName = keys[0];
      const enumColumnName = column_mapping[columnName];

      if (columnName && enumColumnName) {
        relationsMap.push({
          columnName,
          enumTableName: ref_table,
          enumColumnName,
        });
      }
    }
  );

  return relationsMap;
};

export const getTablePermissions = (
  table: Table,
  role: string | null = null,
  action: string | null = null
) => {
  const tablePermissions = table.permissions;

  if (role) {
    const rolePermissions = tablePermissions.find(p => p.role_name === role);

    if (rolePermissions && action) {
      return rolePermissions.permissions[action];
    }
    return null;
  }

  return tablePermissions;
};

export const findTableCheckConstraint = (
  checkConstraints: CheckConstraint[],
  constraintName: string
) => {
  return checkConstraints.find(c => c.constraint_name === constraintName);
};

export const getSchemaTables = (allTables: Table[], tableSchema: string) => {
  return allTables.filter(t => t.table_schema === tableSchema);
};

export const getSchemaTableNames = (
  allTables: Table[],
  tableSchema: string
) => {
  return getSchemaTables(allTables, tableSchema).map(t => t.table_name);
};

export const getTableCustomRootFields = (table: Table) => {
  if (table.configuration) {
    return table.configuration.custom_root_fields || {};
  }
  return {};
};

export const getTableCustomColumnNames = (table: Table) => {
  if (table.configuration) {
    return table.configuration.custom_column_names || {};
  }
  return {};
};
