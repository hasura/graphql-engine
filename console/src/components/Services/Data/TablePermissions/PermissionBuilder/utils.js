export const boolOperators = {
  and: '_and',
  not: '_not',
  or: '_or',
};

export const columnOperators = [
  '_eq',
  '_ne',
  '_in',
  '_nin',
  '_gt',
  '_lt',
  '_gte',
  '_lte',
  '_like',
  '_nlike',
  '_similar',
  '_nsimilar',
  '_is_null',
];

export const arrayColumnOperators = ['_in', '_nin'];

export const boolColumnOperators = ['_is_null'];

export const legacyOperatorsMap = {
  $and: '_and',
  $or: '_or',
  $not: '_not',
  $eq: '_eq',
  $ne: '_ne',
  $in: '_in',
  $nin: '_nin',
  $gt: '_gt',
  $lt: '_lt',
  $gte: '_gte',
  $lte: '_lte',
  $like: '_like',
  $nlike: '_nlike',
  $similar: '_similar',
  $nsimilar: '_nsimilar',
  $is_null: '_is_null',
};

export function isNotOperator(value) {
  return value === boolOperators.not;
}

export function isAndOrOperator(value) {
  return value === boolOperators.or || value === boolOperators.and;
}

export function isArrayColumnOperator(value) {
  return arrayColumnOperators.indexOf(value) !== -1;
}

export function isBoolColumnOperator(value) {
  return boolColumnOperators.indexOf(value) !== -1;
}

export function isColumnOperator(value) {
  return columnOperators.indexOf(value) !== -1;
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

  if (rel.rel_type === 'array') {
    if (rel.rel_def.foreign_key_constraint_on) {
      _refTable = rel.rel_def.foreign_key_constraint_on.table;
    } else if (rel.rel_def.manual_configuration) {
      _refTable = rel.rel_def.manual_configuration.remote_table;
    }
  }

  if (rel.rel_type === 'object') {
    if (rel.rel_def.foreign_key_constraint_on) {
      const fkCol = rel.rel_def.foreign_key_constraint_on;

      for (let i = 0; i < tableSchema.foreign_key_constraints.length; i++) {
        const fkConstraint = tableSchema.foreign_key_constraints[i];
        if (fkCol === Object.keys(fkConstraint.column_mapping)[0]) {
          _refTable = fkConstraint.ref_table;
          break;
        }
      }
    } else if (rel.rel_def.manual_configuration) {
      _refTable = rel.rel_def.manual_configuration.remote_table;
    }
  }

  if (typeof _refTable === 'string') {
    _refTable = getTableDef(_refTable, 'public');
  }

  return _refTable;
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
