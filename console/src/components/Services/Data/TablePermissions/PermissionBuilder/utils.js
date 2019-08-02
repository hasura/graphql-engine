/* Constants */

// TODO: generate using SQL query to handle all types
export const PGTypes = {
  boolean: ['boolean'],
  character: ['character', 'character varying', 'text', 'citext'],
  dateTime: [
    'timestamp',
    'timestamp with time zone',
    'date',
    'time',
    'time with time zone',
    'interval',
  ],
  geometry: ['geometry'],
  geography: ['geography'],
  json: ['json', 'jsonb'],
  numeric: [
    'smallint',
    'integer',
    'bigint',
    'decimal',
    'numeric',
    'real',
    'double precision',
  ],
  uuid: ['uuid'],
};

const boolOperatorsInfo = {
  _and: {
    type: 'array',
  },
  _or: {
    type: 'array',
  },
  _not: {
    type: 'object',
  },
};

const columnOperatorsInfo = {
  _eq: {
    type: 'object',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _ne: {
    type: 'object',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _in: {
    type: 'array',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _nin: {
    type: 'array',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _gt: {
    type: 'object',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _lt: {
    type: 'object',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _gte: {
    type: 'object',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _lte: {
    type: 'object',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _is_null: {
    type: 'object',
    inputType: 'boolean',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _ceq: {
    type: 'object',
    inputType: 'column',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _cne: {
    type: 'object',
    inputType: 'column',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _cgt: {
    type: 'object',
    inputType: 'column',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _clt: {
    type: 'object',
    inputType: 'column',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _cgte: {
    type: 'object',
    inputType: 'column',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _clte: {
    type: 'object',
    inputType: 'column',
    PGTypes: ['boolean', 'character', 'dateTime', 'numeric', 'uuid'],
  },
  _like: {
    type: 'object',
    PGTypes: ['character'],
  },
  _nlike: {
    type: 'object',
    PGTypes: ['character'],
  },
  _ilike: {
    type: 'object',
    PGTypes: ['character'],
  },
  _nilike: {
    type: 'object',
    PGTypes: ['character'],
  },
  _similar: {
    type: 'object',
    PGTypes: ['character'],
  },
  _nsimilar: {
    type: 'object',
    PGTypes: ['character'],
  },
  _contains: {
    type: 'object',
    PGTypes: ['json'],
  },
  _contained_in: {
    type: 'object',
    PGTypes: ['json'],
  },
  _has_key: {
    type: 'object',
    inputType: 'character',
    PGTypes: ['json'],
  },
  _has_keys_any: {
    type: 'array',
    inputType: 'character',
    PGTypes: ['json'],
  },
  _has_keys_all: {
    type: 'array',
    inputType: 'character',
    PGTypes: ['json'],
  },
  _st_contains: {
    type: 'object',
    PGTypes: ['geometry'],
  },
  _st_crosses: {
    type: 'object',
    inputType: 'json',
    PGTypes: ['geometry'],
  },
  _st_equals: {
    type: 'object',
    inputType: 'json',
    PGTypes: ['geometry'],
  },
  _st_overlaps: {
    type: 'object',
    inputType: 'json',
    PGTypes: ['geometry'],
  },
  _st_touches: {
    type: 'object',
    inputType: 'json',
    PGTypes: ['geometry'],
  },
  _st_within: {
    type: 'object',
    inputType: 'json',
    PGTypes: ['geometry'],
  },
  _st_d_within: {
    type: 'object',
    inputType: 'json',
    PGTypes: ['geometry', 'geography'],
  },
  _st_intersects: {
    type: 'object',
    inputType: 'json',
    PGTypes: ['geometry', 'geography'],
  },
};

const getPGTypesOperators = () => {
  const _PGTypesOperators = {};

  Object.keys(columnOperatorsInfo).forEach(op => {
    columnOperatorsInfo[op].PGTypes.forEach(type => {
      _PGTypesOperators[type] = _PGTypesOperators[type] || [];
      _PGTypesOperators[type].push(op);
    });
  });

  return _PGTypesOperators;
};

export const PGTypesOperators = getPGTypesOperators();

export const boolOperators = Object.keys(boolOperatorsInfo);

const columnOperators = Object.keys(columnOperatorsInfo);

export const allOperators = boolOperators.concat(columnOperators);

/* Util functions */

export const isBoolOperator = operator => {
  return boolOperators.includes(operator);
};

export const isArrayBoolOperator = operator => {
  const arrayBoolOperators = Object.keys(boolOperatorsInfo).filter(
    op => boolOperatorsInfo[op].type === 'array'
  );

  return arrayBoolOperators.includes(operator);
};

export const isColumnOperator = operator => {
  return columnOperators.includes(operator);
};

export const isArrayColumnOperator = operator => {
  const arrayColumnOperators = Object.keys(columnOperatorsInfo).filter(
    op => columnOperatorsInfo[op].type === 'array'
  );

  return arrayColumnOperators.includes(operator);
};

export const getOperatorInputType = operator => {
  return columnOperatorsInfo[operator]
    ? columnOperatorsInfo[operator].inputType
    : null;
};

export const getRootPGType = type => {
  let rootType;

  for (const rType of Object.keys(PGTypes)) {
    if (PGTypes[rType].includes(type)) {
      rootType = rType;
      break;
    }
  }

  return rootType;
};

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
          _refTable = getTableDef(
            fkConstraint.ref_table,
            fkConstraint.ref_table_table_schema
          );
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

  const columnSchema = tableSchema.columns.find(
    _columnSchema => _columnSchema.column_name === columnName
  );

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
