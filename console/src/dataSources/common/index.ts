import { isEqual } from '../../components/Common/utils/jsUtils';
import { Nullable } from '../../components/Common/utils/tsUtils';
import { QualifiedTable } from '../../metadata/types';
import { FixMe } from '../../types';
import { BaseTable, CheckConstraint, Relationship, Table } from '../types';

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
): QualifiedTable => {
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

export const getQualifiedTableDef = (
  tableDef: QualifiedTable | string,
  driver?: string
) => {
  if (typeof tableDef === 'string') return generateTableDef(tableDef);

  if (driver === 'bigquery')
    return {
      name: tableDef.name,
      dataset: tableDef.schema,
    };

  return tableDef;
};

export const getTableNameWithSchema = (
  tableDef: QualifiedTable,
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

export const findTable = (allTables: Table[], tableDef: QualifiedTable) => {
  return allTables.find(t => isEqual(getTableDef(t), tableDef));
};

export const getTrackedTables = (tables: Table[]) => {
  return tables.filter(t => t.is_table_tracked);
};

export const getUntrackedTables = (tables: Table[]) => {
  return tables
    .filter(t => !t.is_table_tracked)
    .sort((a, b) => (a.table_name > b.table_name ? 1 : -1));
};

export const getTableColumnNames = (table: Table) => {
  return table.columns.map(c => c.column_name);
};

export function escapeTableColumns(table: Table) {
  if (!table) return {};
  const pattern = /\W+|^\d/;
  return getTableColumnNames(table)
    .filter(col => pattern.test(col))
    .reduce((acc: Record<string, string>, col) => {
      let newColName = col.replace(/\W+/g, '_');
      if (/^\d/.test(newColName)) newColName = `column_${newColName}`;
      acc[col] = newColName;
      return acc;
    }, {});
}

export function escapeTableName(tableName: string): Nullable<string> {
  const pattern = /\W+|^\d/;
  if (!pattern.test(tableName)) return null;
  if (/^\d/.test(tableName)) tableName = `table_${tableName}`;
  return tableName.toLowerCase().replace(/\s+|_?\W+_?/g, '_');
}

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
): QualifiedTable | null {
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

      // if one-to-one relationship from remote table
      if (fkCol.table) {
        refTable = generateTableDef(fkCol.table.name, fkCol.table.schema);
      } else {
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
  }

  if (typeof refTable === 'string') {
    refTable = generateTableDef(refTable);
  }

  return refTable;
}

export function getTableFromRelationshipChain(
  allTables: Table[],
  startTable: Table,
  chains: string
): Table {
  const rels = chains
    .split(/\./g)
    .filter(it => !it.startsWith('_') && Number.isNaN(parseInt(it, 10)))
    .slice(0, -1);
  return rels.reduce((acc, name) => {
    const rship = getTableRelationship(acc, name)!;
    const rShipDef = getRelationshipRefTable(acc, rship)!;
    return findTable(allTables, rShipDef)!;
  }, startTable);
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

export const getTableCustomName = (table: Table) => {
  if (table.configuration) {
    return table.configuration.custom_name || '';
  }
  return '';
};

export const getTableCustomRootFields = (table: Table) => {
  if (table.configuration) {
    return table.configuration.custom_root_fields || {};
  }
  return {};
};

export const getTableCustomColumnNames = (table: Table) => {
  if (table && table.configuration) {
    return table.configuration.custom_column_names || {};
  }
  return {};
};

export const findFKConstraint = (curTable: Table, column: string[]) => {
  const fkConstraints = curTable.foreign_key_constraints;
  return fkConstraints.find(
    fk =>
      Object.keys(fk.column_mapping).length === column.length &&
      Object.keys(fk.column_mapping).join(',') === column.join(',')
  );
};

export const findOppFKConstraint = (curTable: Table, column: string[]) => {
  const fkConstraints = curTable.opp_foreign_key_constraints;
  return fkConstraints.find(
    fk =>
      Object.keys(fk.column_mapping).length === column.length &&
      Object.keys(fk.column_mapping).join(',') === column.join(',')
  );
};

export const findTableFromRel = (
  schemas: BaseTable[],
  curTable: Table,
  rel: Relationship
) => {
  let rTable: FixMe = null;
  let rSchema = 'public';

  // for view
  if (rel.rel_def.manual_configuration !== undefined) {
    rTable = rel.rel_def.manual_configuration.remote_table;
    if (rTable?.schema) {
      rSchema = rTable.schema;
      rTable = rTable.name;
    }
  }

  // for table
  if (rel.rel_def.foreign_key_constraint_on !== undefined) {
    // for object relationship
    if (rel.rel_type === 'object') {
      const column = [rel.rel_def.foreign_key_constraint_on];
      const fkc = findFKConstraint(curTable, column);
      if (fkc) {
        rTable = fkc.ref_table;
        rSchema = fkc.ref_table_table_schema;
      }
    }

    // for array relationship
    if (
      rel.rel_type === 'array' ||
      (rel.rel_type === 'object' && rel.rel_def.foreign_key_constraint_on.table)
    ) {
      rTable = rel.rel_def.foreign_key_constraint_on.table;
      if (rTable.schema) {
        rSchema = rTable.schema;
        rTable = rTable.name;
      }
    }
  }
  return schemas.find(
    x => x.table_name === rTable && x.table_schema === rSchema
  );
};

export const findAllFromRel = (curTable: Table, rel: Relationship) => {
  const relName = rel.rel_name;
  const lTable = rel.table_name;
  const lSchema = rel.table_schema;
  const isObjRel = rel.rel_type === 'object';
  let lcol: string[] | null = null;
  let rcol: string[] | null = null;
  let rTable: string | null = null;
  let rSchema: string | null = null;

  // for view
  if (rel.rel_def.manual_configuration !== undefined) {
    const rTableConfig = rel.rel_def.manual_configuration.remote_table;
    if (rTableConfig.schema) {
      rTable = rTableConfig.name;
      rSchema = rTableConfig.schema;
    } else {
      rTable = rTableConfig;
      rSchema = 'public';
    }
    const columnMapping = rel.rel_def.manual_configuration.column_mapping;
    lcol = Object.keys(columnMapping);
    rcol = lcol.map(column => columnMapping[column]);
  }

  // for table
  const foreignKeyConstraintOn = rel.rel_def.foreign_key_constraint_on;
  if (foreignKeyConstraintOn !== undefined) {
    // for object relationship
    if (
      rel.rel_type === 'object' &&
      typeof foreignKeyConstraintOn === 'string'
    ) {
      lcol = [foreignKeyConstraintOn];
      const fkc = findFKConstraint(curTable, lcol);
      if (fkc) {
        rTable = fkc.ref_table;
        rSchema = fkc.ref_table_table_schema;
        rcol = [fkc ? fkc.column_mapping[(lcol as any) as string] : ''];
      }
    }

    // for array relationship or one-to-one relationship with remote table
    if (
      rel.rel_type === 'array' ||
      (rel.rel_type === 'object' && foreignKeyConstraintOn.column)
    ) {
      rcol = [foreignKeyConstraintOn.column];
      const rTableConfig = foreignKeyConstraintOn.table;
      if (rTableConfig.schema) {
        rTable = rTableConfig.name;
        rSchema = rTableConfig.schema;
      } else {
        rTable = rTableConfig;
        rSchema = 'public';
      }
      const rfkc = findOppFKConstraint(curTable, rcol);
      lcol = [rfkc ? rfkc!.column_mapping[(rcol as any) as string] : ''];
    }
  }
  return {
    relName,
    lTable,
    lSchema,
    isObjRel,
    lcol,
    rcol,
    rTable,
    rSchema,
  };
};

export const terminateSql = (sql: string) => {
  const sqlSanitised = sql.trim();
  return sqlSanitised[sqlSanitised.length - 1] !== ';'
    ? `${sqlSanitised};`
    : sqlSanitised;
};

export const getCheckConstraintBoolExp = (check: string) => {
  if (check) {
    return check.substring(7, check.length - 1);
  }

  return check;
};

export * from './graphqlUtil';
