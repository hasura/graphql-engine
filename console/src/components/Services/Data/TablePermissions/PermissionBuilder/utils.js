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

  return tableSchema.relationships[
    getTableRelationshipNames(tableSchema).indexOf(relName)
  ];
}

export function getRefTable(rel, schema) {
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

      for (let i = 0; i < schema.foreign_key_constraints.length; i++) {
        const fkConstraint = schema.foreign_key_constraints[i];
        if (fkCol === Object.keys(fkConstraint.column_mapping)[0]) {
          _refTable = fkConstraint.ref_table;
          break;
        }
      }
    } else if (rel.rel_def.manual_configuration) {
      _refTable = rel.rel_def.manual_configuration.remote_table;
    }
  }

  return _refTable;
}
