/* Constants */

export const PGTypes = {
  boolean: [
    'boolean'
  ],
  uuid: [
    'uuid'
  ],
  numeric: [
    'smallint',
    'integer',
    'bigint',
    'decimal',
    'numeric',
    'real',
    'double precision',
  ],
  character: [
    'character',
    'character varying',
    'text'
  ],
  dateTime: [
    'timestamp',
    'timestamp with time zone',
    'date',
    'time',
    'time with time zone',
    'interval'
  ],
  json: [
    'json',
    'jsonb'
  ],
  postgis: [
    'geometry',
    'geography'
  ]
};

export const notBoolOperators = [
  '_not'
];

export const andOrBoolOperators = [
  '_and',
  '_or'
];

export const genericSimpleColumnOperators = [
  '_eq',
  '_ne',
  '_in',
  '_nin',
  '_gt',
  '_lt',
  '_gte',
  '_lte'
];

export const genericArrayColumnOperators = [
  '_in',
  '_nin'
];

export const genericBoolColumnOperators = [
  '_is_null'
];

export const textOnlyColumnOperators = [
  '_like',
  '_nlike',
  '_ilike',
  '_nilike',
  '_similar',
  '_nsimilar',
];

export const jsonColumnOperators = [
  '_contains',
  '_contained_in'
];

export const topologyColumnOperators = [
  '_st_contains',
  '_st_crosses',
  '_st_equals',
  '_st_intersects',
  '_st_overlaps',
  '_st_touches',
  '_st_within',
  '_st_d_within'
];

export const boolOperators = notBoolOperators
  .concat(andOrBoolOperators);

export const columnOperators = genericSimpleColumnOperators
  .concat(genericArrayColumnOperators)
  .concat(genericBoolColumnOperators)
  .concat(textOnlyColumnOperators)
  .concat(jsonColumnOperators)
  .concat(topologyColumnOperators);

export const genericOperators = genericSimpleColumnOperators
  .concat(genericArrayColumnOperators)
  .concat(genericBoolColumnOperators);

export const textColumnOperators = genericOperators
  .concat(textOnlyColumnOperators);

export const allOperators = boolOperators
  .concat(columnOperators);

/* Util functions */

export function isNotOperator(value) {
  return notBoolOperators.includes(value);
}

export function isAndOrOperator(value) {
  return andOrBoolOperators.includes(value);
}

export function isArrayTypeColumnOperator(value) {
  return genericArrayColumnOperators.includes(value);
}

export function isBoolTypeColumnOperator(value) {
  return genericBoolColumnOperators.includes(value);
}

export function isJsonTypeColumnOperator(value) {
  return jsonColumnOperators
    .concat(topologyColumnOperators)
    .includes(value);
}

export function isColumnOperator(value) {
  return columnOperators.includes(value);
}

export function getLegacyOperator(operator) {
  return operator.replace('_', '$');
}

export function addToPrefix(prefix, value) {
  let _newPrefix;
  if (prefix !== null && prefix.toString()) {
    if (
      prefix[prefix.length - 1] === '^' ||
      prefix[prefix.length - 1] === '#'
    ) {
      _newPrefix = prefix + value;
    } else {
      _newPrefix = prefix + '.' + value;
    }
  } else {
    _newPrefix = value;
  }

  return _newPrefix;
}

export function getTableSchema(allSchemas, table) {
  return allSchemas.find(
    tableSchema =>
      tableSchema.table_name === table.name &&
      tableSchema.table_schema === table.schema
  );
}

export function getTableColumnNames(tableSchema) {
  if (!tableSchema) {
    return [];
  }

  return tableSchema.columns.map(c => c.column_name);
}

export function getTableRelationshipNames(tableSchema) {
  if (!tableSchema) {
    return [];
  }

  return tableSchema.relationships.map(r => r.rel_name);
}

export function getTableRelationship(tableSchema, relName) {
  if (!tableSchema) {
    return {};
  }

  const relIndex = getTableRelationshipNames(tableSchema).indexOf(relName);

  return tableSchema.relationships[relIndex];
}

export function getTableDef(tableName, schema) {
  return { name: tableName, schema: schema };
}

export function getRefTable(rel, tableSchema) {
  let _refTable = null;

  // if manual relationship
  if (rel.rel_def.manual_configuration) {
    _refTable = rel.rel_def.manual_configuration.remote_table;
  }

  // if foreign-key based relationship
  if (rel.rel_def.foreign_key_constraint_on) {
    // if array relationship
    if (rel.rel_type === 'array') {
      _refTable = rel.rel_def.foreign_key_constraint_on.table;
    }

    // if object relationship
    if (rel.rel_type === 'object') {
      const fkCol = rel.rel_def.foreign_key_constraint_on;

      for (let i = 0; i < tableSchema.foreign_key_constraints.length; i++) {
        const fkConstraint = tableSchema.foreign_key_constraints[i];
        const fkConstraintCol = Object.keys(fkConstraint.column_mapping)[0];
        if (fkCol === fkConstraintCol) {
          _refTable = getTableDef(fkConstraint.ref_table, fkConstraint.ref_table_table_schema);
          break;
        }
      }
    }
  }

  if (typeof _refTable === 'string') {
    _refTable = getTableDef(_refTable, 'public');
  }

  return _refTable;
}

export function getColumnType(columnName, tableSchema) {
  let _columnType = '';

  if (!tableSchema || !columnName) {
    return _columnType;
  }

  const columnSchema = tableSchema.columns.find(_columnSchema => (_columnSchema.column_name === columnName));

  if (columnSchema) {
    _columnType = columnSchema.data_type;

    if (_columnType === 'USER-DEFINED') {
      _columnType = columnSchema.udt_name;
    }
  }

  return _columnType;
}

export function isJsonString(str) {
  try {
    JSON.parse(str);
  } catch (e) {
    return false;
  }

  return true;
}

export function getAllJsonPaths(json, prefix = '') {
  const _paths = [];

  const addPrefix = subPath => {
    return prefix + (prefix && subPath ? '.' : '') + subPath;
  };

  const handleSubJson = (subJson, newPrefix) => {
    const subPaths = getAllJsonPaths(subJson, newPrefix);

    subPaths.forEach(subPath => {
      _paths.push(subPath);
    });

    if (!subPaths.length) {
      _paths.push(newPrefix);
    }
  };

  if (json instanceof Array) {
    json.forEach((subJson, i) => {
      handleSubJson(subJson, addPrefix(i.toString()));
    });
  } else if (json instanceof Object) {
    Object.keys(json).forEach(key => {
      handleSubJson(json[key], addPrefix(key));
    });
  } else {
    _paths.push(addPrefix(json));
  }

  return _paths;
}
