import { Operator } from '../../types';

export const getSupportedOperators = async () => {
  const graphqlOps: Operator[] = [
    {
      name: 'equals',
      value: '$eq',
    },
    {
      name: 'not equals',
      value: '$ne',
    },
    {
      name: 'in',
      value: '$in',
      defaultValue: '[]',
    },
    {
      name: 'not in',
      value: '$nin',
      defaultValue: '[]',
    },
    {
      name: '>',
      value: '$gt',
    },
    {
      name: '<',
      value: '$lt',
    },
    {
      name: '>=',
      value: '$gte',
    },
    {
      name: '<=',
      value: '$lte',
    },
    {
      name: 'like',
      value: '$like',
      defaultValue: '%%',
    },
    {
      name: 'not like',
      value: '$nlike',
      defaultValue: '%%',
    },
    {
      name: 'like (case-insensitive)',
      value: '$ilike',
      defaultValue: '%%',
    },
    {
      name: 'not like (case-insensitive)',
      value: '$nilike',
      defaultValue: '%%',
    },
    {
      name: 'similar',
      value: '$similar',
    },
    {
      name: 'not similar',
      value: '$nsimilar',
    },
    {
      name: '~',
      value: '$regex',
    },
    {
      name: '~*',
      value: '$iregex',
    },
    {
      name: '!~',
      value: '$nregex',
    },
    {
      name: '!~*',
      value: '$niregex',
    },
  ];
  return graphqlOps;
};
