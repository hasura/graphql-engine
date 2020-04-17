import React from 'react';
import { isEqual, isString } from './jsUtils';

/*** Table/View utils ***/

export const getTableName = table => {
  return table.table_name;
};

export const getTableSchema = table => {
  return table.table_schema;
};

// TODO: figure out better pattern for overloading fns
// tableName and tableNameWithSchema are either/or arguments
export const generateTableDef = (
  tableName,
  tableSchema = 'public',
  tableNameWithSchema = null
) => {
  if (tableNameWithSchema) {
    tableSchema = tableNameWithSchema.split('.')[0];
    tableName = tableNameWithSchema.split('.')[1];
  }

  return {
    schema: tableSchema,
    name: tableName,
  };
};

export const getTableDef = table => {
  return generateTableDef(getTableName(table), getTableSchema(table));
};

export const getQualifiedTableDef = tableDef => {
  return isString(tableDef) ? generateTableDef(tableDef) : tableDef;
};

export const getTableNameWithSchema = (tableDef, wrapDoubleQuotes = false) => {
  let _fullTableName;

  if (wrapDoubleQuotes) {
    _fullTableName =
      '"' + tableDef.schema + '"' + '.' + '"' + tableDef.name + '"';
  } else {
    _fullTableName = tableDef.schema + '.' + tableDef.name;
  }

  return _fullTableName;
};

export const checkIfTable = table => {
  return table.table_type === 'BASE TABLE';
};

export const displayTableName = table => {
  const tableName = getTableName(table);
  const isTable = checkIfTable(table);

  return isTable ? <span>{tableName}</span> : <i>{tableName}</i>;
};

export const findTable = (allTables, tableDef) => {
  return allTables.find(t => isEqual(getTableDef(t), tableDef));
};

export const getTrackedTables = tables => {
  return tables.filter(t => t.is_table_tracked);
};

export const getUntrackedTables = tables => {
  return tables.filter(t => !t.is_table_tracked);
};

export const getOnlyTables = tablesOrViews => {
  return tablesOrViews.filter(t => checkIfTable(t));
};

export const getOnlyViews = tablesOrViews => {
  return tablesOrViews.filter(t => !checkIfTable(t));
};

export const QUERY_TYPES = ['insert', 'select', 'update', 'delete'];

export const getTableSupportedQueries = table => {
  let supportedQueryTypes;

  if (checkIfTable(table)) {
    supportedQueryTypes = QUERY_TYPES;
  } else { // is View
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

/*** Table/View column utils ***/

export const getTableColumns = table => {
  return table.columns;
};

export const getColumnName = column => {
  return column.column_name;
};

export const getTableColumnNames = table => {
  return getTableColumns(table).map(c => getColumnName(c));
};

export const getTableColumn = (table, columnName) => {
  return getTableColumns(table).find(
    column => getColumnName(column) === columnName
  );
};

export const getColumnType = column => {
  let _columnType = column.data_type;

  if (_columnType === 'USER-DEFINED') {
    _columnType = column.udt_name;
  }

  return _columnType;
};

export const isColumnAutoIncrement = column => {
  const columnDefault = column.column_default;

  const autoIncrementDefaultRegex = /^nextval\('(.*)_seq'::regclass\)$/;

  return (
    columnDefault &&
    columnDefault.match(new RegExp(autoIncrementDefaultRegex, 'gi'))
  );
};

/*** Table/View relationship utils ***/

export const getTableRelationships = table => {
  return table.relationships;
};

export const getRelationshipName = relationship => {
  return relationship.rel_name;
};

export const getRelationshipDef = relationship => {
  return relationship.rel_def;
};

export const getRelationshipType = relationship => {
  return relationship.rel_type;
};

export const getTableRelationshipNames = table => {
  return getTableRelationships(table).map(r => getRelationshipName(r));
};

export function getTableRelationship(table, relationshipName) {
  return getTableRelationships(table).find(
    relationship => getRelationshipName(relationship) === relationshipName
  );
}

export function getRelationshipRefTable(table, relationship) {
  let _refTable = null;

  const relationshipDef = getRelationshipDef(relationship);
  const relationshipType = getRelationshipType(relationship);

  // if manual relationship
  if (relationshipDef.manual_configuration) {
    _refTable = relationshipDef.manual_configuration.remote_table;
  }

  // if foreign-key based relationship
  if (relationshipDef.foreign_key_constraint_on) {
    // if array relationship
    if (relationshipType === 'array') {
      _refTable = relationshipDef.foreign_key_constraint_on.table;
    }

    // if object relationship
    if (relationshipType === 'object') {
      const fkCol = relationshipDef.foreign_key_constraint_on;

      for (let i = 0; i < table.foreign_key_constraints.length; i++) {
        const fkConstraint = table.foreign_key_constraints[i];
        const fkConstraintCol = Object.keys(fkConstraint.column_mapping)[0];
        if (fkCol === fkConstraintCol) {
          _refTable = generateTableDef(
            fkConstraint.ref_table,
            fkConstraint.ref_table_table_schema
          );
          break;
        }
      }
    }
  }

  if (typeof _refTable === 'string') {
    _refTable = generateTableDef(_refTable);
  }

  return _refTable;
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
export const getEnumColumnMappings = (allSchemas, tableName, tableSchema) => {
  const currentTable = findTable(
    allSchemas,
    generateTableDef(tableName, tableSchema)
  );

  const relationsMap = [];
  if (!currentTable.foreign_key_constraints.length) return;

  currentTable.foreign_key_constraints.map(
    ({ ref_table, ref_table_table_schema, column_mapping }) => {
      const refTableSchema = findTable(
        allSchemas,
        generateTableDef(ref_table, ref_table_table_schema)
      );
      if (!refTableSchema || !refTableSchema.is_enum) return;

      const keys = Object.keys(column_mapping);
      if (!keys.length) return;

      const _columnName = keys[0];
      const _enumColumnName = column_mapping[_columnName];

      if (_columnName && _enumColumnName) {
        relationsMap.push({
          columnName: _columnName,
          enumTableName: ref_table,
          enumColumnName: _enumColumnName,
        });
      }
    }
  );

  return relationsMap;
};

/*** Table/View permissions utils ***/

export const getTablePermissions = (table, role = null, action = null) => {
  let tablePermissions = table.permissions;

  if (role) {
    tablePermissions = tablePermissions.find(p => p.role_name === role);

    if (tablePermissions && action) {
      tablePermissions = tablePermissions.permissions[action];
    }
  }

  return tablePermissions;
};

/*** Table/View Check Constraints utils ***/

export const getTableCheckConstraints = table => {
  return table.check_constraints;
};

export const getCheckConstraintName = constraint => {
  return constraint.constraint_name;
};

export const findTableCheckConstraint = (checkConstraints, constraintName) => {
  return checkConstraints.find(
    c => getCheckConstraintName(c) === constraintName
  );
};

/*** Function utils ***/

export const getFunctionSchema = pgFunction => {
  return pgFunction.function_schema;
};

export const getFunctionName = pgFunction => {
  return pgFunction.function_name;
};

export const getFunctionDefinition = pgFunction => {
  return pgFunction.function_definition;
};

export const getSchemaFunctions = (allFunctions, fnSchema) => {
  return allFunctions.filter(fn => getFunctionSchema(fn) === fnSchema);
};

export const findFunction = (allFunctions, functionName, functionSchema) => {
  return allFunctions.find(
    f =>
      getFunctionName(f) === functionName &&
      getFunctionSchema(f) === functionSchema
  );
};

/*** Schema utils ***/

export const getSchemaName = schema => {
  return schema.schema_name;
};

export const getSchemaTables = (allTables, tableSchema) => {
  return allTables.filter(t => getTableSchema(t) === tableSchema);
};

export const getSchemaTableNames = (allTables, tableSchema) => {
  return getSchemaTables(allTables, tableSchema).map(t => getTableName(t));
};

/*** Custom table fields utils ***/

export const getTableCustomRootFields = table => {
  if (table.configuration) {
    return table.configuration.custom_root_fields || {};
  }
  return {};
};

export const getTableCustomColumnNames = table => {
  if (table.configuration) {
    return table.configuration.custom_column_names || {};
  }
  return {};
};

/*** Table/View Computed Field utils ***/

export const getTableComputedFields = table => {
  return table.computed_fields;
};

export const getComputedFieldName = computedField => {
  return computedField.computed_field_name;
};

export const getGroupedTableComputedFields = (table, allFunctions) => {
  const groupedComputedFields = { scalar: [], table: [] };

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
