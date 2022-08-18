/* Constants */

import { PermissionColumnCategories } from '@/dataSources/types';

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

export type BoolOperators = keyof typeof boolOperatorsInfo;
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

export type ColumnOperators = keyof typeof columnOperatorsInfo;
const columnOperatorsInfo = {
  _eq: {
    type: 'comparision',
    inputStructure: 'object',
    inputType: null,
  },
  _ne: {
    type: 'comparision',
    inputStructure: 'object',
    inputType: null,
  },
  _neq: {
    type: 'comparision',
    inputStructure: 'object',
    inputType: null,
  },
  _in: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: null,
  },
  _nin: {
    type: 'comparision',
    inputStructure: 'array',
    inputType: null,
  },
  _gt: {
    type: 'comparision',
    inputStructure: 'object',
    inputType: null,
  },
  _lt: {
    type: 'comparision',
    inputStructure: 'object',
    inputType: null,
  },
  _gte: {
    type: 'comparision',
    inputStructure: 'object',
    inputType: null,
  },
  _lte: {
    type: 'comparision',
    inputStructure: 'object',
    inputType: null,
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
    inputType: null,
  },
  _nlike: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _ilike: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _nilike: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _similar: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _nsimilar: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _regex: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _iregex: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _nregex: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _niregex: {
    type: 'pattern_match',
    inputStructure: 'object',
    inputType: null,
  },
  _contains: {
    type: 'jsonb',
    inputStructure: 'object',
    inputType: null,
  },
  _contained_in: {
    type: 'jsonb',
    inputStructure: 'object',
    inputType: null,
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
    inputType: null,
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

export const getPermissionOperators = (
  supportedOperators: ColumnOperators[],
  typeMap: Partial<PermissionColumnCategories> | null
) => {
  let modifiedColumnOperatorsInfo = columnOperatorsInfo;
  if (Array.isArray(supportedOperators)) {
    modifiedColumnOperatorsInfo = supportedOperators.reduce(
      (ops, opName) => ({
        ...ops,
        [opName]: columnOperatorsInfo[opName],
      }),
      {} as typeof columnOperatorsInfo
    );
  }

  const operatorMap = {
    ...operatorTypeTypesMap,
    is_null: Object.keys(typeMap ?? {}),
  };
  type OperatorMapKey = keyof typeof operatorMap;
  const operators: Partial<typeof operatorMap> = {};

  Object.keys(modifiedColumnOperatorsInfo).forEach(op => {
    const key = modifiedColumnOperatorsInfo[op as ColumnOperators].type;
    operatorMap[key as OperatorMapKey].forEach(type => {
      operators[type as OperatorMapKey] =
        operators[type as OperatorMapKey] || [];
      operators[type as OperatorMapKey]?.push(op);
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

export const isBoolOperator = (operator: BoolOperators) => {
  return boolOperators.includes(operator);
};

export const isExistOperator = (operator: string) => {
  return existOperators.includes(operator);
};

export const isArrayBoolOperator = (operator: BoolOperators) => {
  const arrayBoolOperators = Object.keys(boolOperatorsInfo).filter(
    op => boolOperatorsInfo[op as BoolOperators].inputStructure === 'array'
  );

  return arrayBoolOperators.includes(operator);
};

export const isColumnOperator = (operator: ColumnOperators) => {
  return columnOperators.includes(operator);
};

export const isArrayColumnOperator = (operator: string) => {
  const arrayColumnOperators = Object.keys(columnOperatorsInfo).filter(
    op => columnOperatorsInfo[op as ColumnOperators].inputStructure === 'array'
  );

  return arrayColumnOperators.includes(operator);
};

export const getOperatorInputType = (operator: ColumnOperators) => {
  return columnOperatorsInfo[operator]
    ? columnOperatorsInfo[operator].inputType
    : null;
};

export const getRootType = (
  type: string,
  typeMap: Partial<PermissionColumnCategories> | null
) => {
  const typeMapKeys = Object.keys(typeMap ?? {});

  let rootType = typeMapKeys.find(rType =>
    typeMap?.[rType as keyof PermissionColumnCategories]?.includes(type)
  );

  if (!rootType) {
    rootType = 'user_defined';
  }

  return rootType;
};

export function getLegacyOperator(operator: string) {
  return operator.replace('_', '$');
}

export function addToPrefix(prefix: string, value: string | number) {
  let newPrefix;
  if (prefix !== null && prefix.toString()) {
    if (
      prefix[prefix.length - 1] === '^' ||
      prefix[prefix.length - 1] === '#'
    ) {
      newPrefix = prefix + value;
    } else {
      newPrefix = `${prefix}.${value}`;
    }
  } else {
    newPrefix = value as string;
  }

  return newPrefix;
}
