/* Constants */

// TODO: generate using SQL query to handle all types

const operatorTypeTypesMap = {
  comparision: [
    'boolean',
    'character',
    'dateTime',
    'numeric',
    'uuid',
    'user_defined',
  ],
  pattern_match: ['character'],
  jsonb: ['jsonb'],
  geometric: ['geometry'],
  geometric_geographic: ['geometry', 'geography'],
};

const boolOperatorsInfo = {
  _and: {
    type: 'bool',
    inputStructure: 'array',
  },
  _or: {
    type: 'bool',
    inputStructure: 'array',
  },
  _not: {
    type: 'bool',
    inputStructure: 'object',
  },
};

const columnOperatorsInfo = {
  _eq: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _ne: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _neq: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _in: {
    type: 'comparision',
    inputStructure: 'array',
  },
  _nin: {
    type: 'comparision',
    inputStructure: 'array',
  },
  _gt: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _lt: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _gte: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _lte: {
    type: 'comparision',
    inputStructure: 'object',
  },
  _ceq: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _cne: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _cgt: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _clt: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _cgte: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _clte: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: 'column',
  },
  _is_null: {
    type: 'is_null',
    inputStructure: 'object',
    inputType: 'boolean',
  },
  _like: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _nlike: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _ilike: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _nilike: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _similar: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _nsimilar: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _regex: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _iregex: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _nregex: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _niregex: {
    type: 'pattern_match',
    inputStructure: 'object',
  },
  _contains: {
    type: 'jsonb',
    inputStructure: 'object',
  },
  _contained_in: {
    type: 'jsonb',
    inputStructure: 'object',
  },
  _has_key: {
    type: 'jsonb',
    inputStructure: 'object',
    inputType: 'character',
  },
  _has_keys_any: {
    type: 'jsonb',
    inputStructure: 'array',
    inputType: 'character',
  },
  _has_keys_all: {
    type: 'jsonb',
    inputStructure: 'array',
    inputType: 'character',
  },
  _st_contains: {
    type: 'geometric',
    inputStructure: 'object',
  },
  _st_crosses: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_equals: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_overlaps: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_touches: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_within: {
    type: 'geometric',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_d_within: {
    type: 'geometric_geographic',
    inputStructure: 'object',
    inputType: 'json',
  },
  _st_intersects: {
    type: 'geometric_geographic',
    inputStructure: 'object',
    inputType: 'json',
  },
};

export const getPermissionOperators = (supportedOperators, typeMap) => {
  let modifiedColumnOperatorsInfo = columnOperatorsInfo;
  if (Array.isArray(supportedOperators)) {
    modifiedColumnOperatorsInfo = supportedOperators.reduce(
      (ops, opName) => ({
        ...ops,
        [opName]: columnOperatorsInfo[opName],
      }),
      {}
    );
  }

  const operators = {};
  const operatorMap = {
    ...operatorTypeTypesMap,
    is_null: Object.keys(typeMap),
  };

  Object.keys(modifiedColumnOperatorsInfo).forEach(op => {
    operatorMap[modifiedColumnOperatorsInfo[op].type].forEach(type => {
      operators[type] = operators[type] || [];
      operators[type].push(op);
    });
  });

  return operators;
};

export const boolOperators = Object.keys(boolOperatorsInfo);

const columnOperators = Object.keys(columnOperatorsInfo);

export const existOperators = ['_exists'];

export const allOperators = boolOperators
  .concat(columnOperators)
  .concat(existOperators);

export const TABLE_KEY = '_table';
export const WHERE_KEY = '_where';

/* Util functions */

export const isBoolOperator = operator => {
  return boolOperators.includes(operator);
};

export const isExistOperator = operator => {
  return existOperators.includes(operator);
};

export const isArrayBoolOperator = operator => {
  const arrayBoolOperators = Object.keys(boolOperatorsInfo).filter(
    op => boolOperatorsInfo[op].inputStructure === 'array'
  );

  return arrayBoolOperators.includes(operator);
};

export const isColumnOperator = operator => {
  return columnOperators.includes(operator);
};

export const isArrayColumnOperator = operator => {
  const arrayColumnOperators = Object.keys(columnOperatorsInfo).filter(
    op => columnOperatorsInfo[op].inputStructure === 'array'
  );

  return arrayColumnOperators.includes(operator);
};

export const getOperatorInputType = operator => {
  return columnOperatorsInfo[operator]
    ? columnOperatorsInfo[operator].inputType
    : null;
};

export const getRootType = (type, typeMap) => {
  const typeMapKeys = Object.keys(typeMap);

  let rootType = typeMapKeys.find(rType => typeMap[rType].includes(type));

  if (!rootType) {
    rootType = 'user_defined';
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
