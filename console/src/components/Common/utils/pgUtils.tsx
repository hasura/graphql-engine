import React from 'react';
import { isEqual, isString } from './jsUtils';
import { TableDefinition, FunctionDefinition } from './v1QueryUtils';

/** * Table/View utils ** */

export type TableRelationship = {
  rel_name: string;
  rel_def: {
    manual_configuration?: any;
    foreign_key_constraint_on?: any; // TODO
  };
  rel_type: 'object' | 'array';
};

export type TablePermission = {
  role_name: string;
  permissions: {
    [action: string]: any;
  };
};

export type TableColumn = {
  column_name: string;
  data_type: string;
  udt_name: string;
  column_default: string;
};

export type ForeignKeyConstraint = {
  ref_table: string;
  ref_table_table_schema: string;
  column_mapping: {
    [lcol: string]: string;
  };
};

export type CheckConstraint = {
  constraint_name: string;
  check: string;
};

export type ComputedField = {
  computed_field_name: string;
  definition: {
    function: FunctionDefinition;
  };
};

export type Schema = {
  schema_name: string;
};

export type Table = {
  table_name: string;
  table_schema: string;
  table_type: string;
  is_table_tracked: boolean;
  columns: TableColumn[];
  relationships: TableRelationship[];
  permissions: TablePermission[];
  foreign_key_constraints: ForeignKeyConstraint[];
  check_constraints: CheckConstraint[];
  configuration?: {
    custom_column_names: {
      [column: string]: string;
    };
    custom_root_fields: {
      select?: string;
      select_by_pk?: string;
      select_aggregate?: string;
      insert?: string;
      insert_one?: string;
      update?: string;
      update_by_pk?: string;
      delete?: string;
      delete_by_pk?: string;
    };
  };
  computed_fields: ComputedField[];
  is_enum: boolean;
  view_info: {
    is_trigger_insertable_into: 'YES' | 'NO';
    is_insertable_into: 'YES' | 'NO';
    is_updatable: 'YES' | 'NO';
    is_trigger_updatable: 'YES' | 'NO';
    is_trigger_deletable: 'YES' | 'NO';
  };
};

export type PGFunction = {
  function_name: string;
  function_schema: string;
  function_definition: string;
  return_type_type: string;
};

export type PGSchema = {
  schema_name: string;
};

export const getTableName = (table: Table) => {
  return table.table_name;
};

export const getTableSchema = (table: Table) => {
  return table.table_schema;
};

export const getTableType = (table: Table) => {
  return table.table_type;
};

// TODO: figure out better pattern for overloading fns
// tableName and tableNameWithSchema are either/or arguments
export const generateTableDef = (
  tableName: string,
  tableSchema = 'public',
  tableNameWithSchema: string | null = null
) => {
  if (tableNameWithSchema) {
    return {
      schema: tableNameWithSchema.split('.')[0],
      name: tableNameWithSchema.split('.')[1],
    };
  }
  return {
    schema: tableSchema,
    name: tableName,
  };
};

export const getTableDef = (table: Table) => {
  return generateTableDef(getTableName(table), getTableSchema(table));
};

export const getQualifiedTableDef = (tableDef: TableDefinition | string) => {
  return isString(tableDef) ? generateTableDef(tableDef as string) : tableDef;
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

export const checkIfTable = (table: Table) => {
  return table.table_type === 'TABLE';
};

export const displayTableName = (table: Table) => {
  const tableName = getTableName(table);
  const isTable = checkIfTable(table);

  return isTable ? <span>{tableName}</span> : <i>{tableName}</i>;
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

export const getOnlyTables = (tablesOrViews: Table[]) => {
  return tablesOrViews.filter(t => checkIfTable(t));
};

export const getOnlyViews = (tablesOrViews: Table[]) => {
  return tablesOrViews.filter(t => !checkIfTable(t));
};

export const QUERY_TYPES = ['insert', 'select', 'update', 'delete'];

export const getTableSupportedQueries = (table: Table) => {
  let supportedQueryTypes;

  if (checkIfTable(table)) {
    supportedQueryTypes = QUERY_TYPES;
  } else {
    // is View
    supportedQueryTypes = [];

    // Add insert/update permission if it is insertable/updatable as returned by pg
    if (table.view_info) {
      if (
        table.view_info.is_insertable_into === 'YES' ||
        table.view_info.is_trigger_insertable_into === 'YES'
      ) {
        supportedQueryTypes.push('insert');
      }

      supportedQueryTypes.push('select'); // to maintain order

      if (table.view_info.is_updatable === 'YES') {
        supportedQueryTypes.push('update');
        supportedQueryTypes.push('delete');
      } else {
        if (table.view_info.is_trigger_updatable === 'YES') {
          supportedQueryTypes.push('update');
        }

        if (table.view_info.is_trigger_deletable === 'YES') {
          supportedQueryTypes.push('delete');
        }
      }
    } else {
      supportedQueryTypes.push('select');
    }
  }

  return supportedQueryTypes;
};

/** * Table/View column utils ** */

export const getTableColumns = (table?: Table) => {
  return table ? table.columns : [];
};

export const getColumnName = (column: TableColumn) => {
  return column.column_name;
};

export const getTableColumnNames = (table: Table) => {
  return getTableColumns(table).map(c => getColumnName(c));
};

export const getTableColumn = (table: Table, columnName: string) => {
  return getTableColumns(table).find(
    column => getColumnName(column) === columnName
  );
};

export const getColumnType = (column: TableColumn) => {
  let columnType = column.data_type;

  if (columnType === 'USER-DEFINED') {
    columnType = column.udt_name;
  }

  return columnType;
};

export const isColumnAutoIncrement = (column: TableColumn) => {
  const columnDefault = column.column_default;

  const autoIncrementDefaultRegex = /^nextval\('(.*)_seq'::regclass\)$/;

  return (
    columnDefault &&
    columnDefault.match(new RegExp(autoIncrementDefaultRegex, 'gi'))
  );
};

/** * Table/View relationship utils ** */

export const getTableRelationships = (table: Table) => {
  return table.relationships;
};

export const getRelationshipName = (relationship: TableRelationship) => {
  return relationship.rel_name;
};

export const getRelationshipDef = (relationship: TableRelationship) => {
  return relationship.rel_def;
};

export const getRelationshipType = (relationship: TableRelationship) => {
  return relationship.rel_type;
};

export const getTableRelationshipNames = (table: Table) => {
  return getTableRelationships(table).map(r => getRelationshipName(r));
};

export function getTableRelationship(table: Table, relationshipName: string) {
  return getTableRelationships(table).find(
    relationship => getRelationshipName(relationship) === relationshipName
  );
}

export function getRelationshipRefTable(
  table: Table,
  relationship: TableRelationship
) {
  let refTable = null;

  const relationshipDef = getRelationshipDef(relationship);
  const relationshipType = getRelationshipType(relationship);

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

/**
 * @param {string} currentSchema
 * @param {string} currentTable
 * @param {Array<{[key: string]: any}>} allSchemas
 *
 * @returns {Array<{
 *   columnName: string,
 *   enumTableName: string,
 *   enumColumnName: string,
 * }>}
 */

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

/** * Table/View permissions utils ** */

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

/** * Table/View Check Constraints utils ** */

export const getTableCheckConstraints = (table: Table) => {
  return table.check_constraints;
};

export const getCheckConstraintName = (constraint: CheckConstraint) => {
  return constraint.constraint_name;
};

export const findTableCheckConstraint = (
  checkConstraints: CheckConstraint[],
  constraintName: string
) => {
  return checkConstraints.find(
    c => getCheckConstraintName(c) === constraintName
  );
};

/** * Function utils ** */

export const getFunctionSchema = (pgFunction: PGFunction) => {
  return pgFunction.function_schema;
};

export const getFunctionName = (pgFunction: PGFunction) => {
  return pgFunction.function_name;
};

export const getFunctionDefinition = (pgFunction: PGFunction) => {
  return pgFunction.function_definition;
};

export const getSchemaFunctions = (
  allFunctions: PGFunction[],
  fnSchema: string
) => {
  return allFunctions.filter(fn => getFunctionSchema(fn) === fnSchema);
};

export const findFunction = (
  allFunctions: PGFunction[],
  functionName: string,
  functionSchema: string
) => {
  return allFunctions.find(
    f =>
      getFunctionName(f) === functionName &&
      getFunctionSchema(f) === functionSchema
  );
};

/** * Schema utils ** */

export const getSchemaName = (schema: PGSchema) => {
  return schema.schema_name;
};

export const getSchemaTables = (allTables: Table[], tableSchema: string) => {
  return allTables.filter(t => getTableSchema(t) === tableSchema);
};

export const getSchemaTableNames = (
  allTables: Table[],
  tableSchema: string
) => {
  return getSchemaTables(allTables, tableSchema).map(t => getTableName(t));
};

/** * Custom table fields utils ** */

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

/** * Table/View Computed Field utils ** */

export const getTableComputedFields = (table: Table) => {
  return table.computed_fields;
};

export const getComputedFieldName = (computedField: ComputedField) => {
  return computedField.computed_field_name;
};

export const getGroupedTableComputedFields = (
  table: Table,
  allFunctions: PGFunction[]
) => {
  const groupedComputedFields: {
    scalar: ComputedField[];
    table: ComputedField[];
  } = { scalar: [], table: [] };

  getTableComputedFields(table).forEach(computedField => {
    const computedFieldFnDef = computedField.definition.function;
    const computedFieldFn = findFunction(
      allFunctions,
      computedFieldFnDef.name,
      computedFieldFnDef.schema
    );

    if (computedFieldFn && computedFieldFn.return_type_type === 'b') {
      groupedComputedFields.scalar.push(computedField);
    } else {
      groupedComputedFields.table.push(computedField);
    }
  });

  return groupedComputedFields;
};

// export const getDependentTables = (table) => {

//   return [
//     {
//       table_schema: table.table_schema,
//       table_name: table.table_name,
//     },
//     ...table.foreign_key_constraints.map(fk_obj => ({
//       table_name: fk_obj.ref_table,
//       table_schema: fk_obj.ref_table_table_schema
//     })),
//     ...table.opp_foreign_key_constraints.map(fk_obj => ({
//       table_name: fk_obj.table_name,
//       table_schema: fk_obj.table_schema,
//     }))
//   ]

// };
