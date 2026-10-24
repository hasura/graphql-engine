import { Operator } from '../../types';

export const getSupportedOperators = () => {
  const graphqlOps: Operator[] = [
    {
      name: 'equals',
      value: '_eq',
    },
    {
      name: 'not equals',
      value: '_neq',
    },
    {
      name: 'in',
      value: '_in',
      defaultValue: '[]',
    },
    {
      name: 'not in',
      value: '_nin',
      defaultValue: '[]',
    },
    {
      name: '>',
      value: '_gt',
    },
    {
      name: '<',
      value: '_lt',
    },
    {
      name: '>=',
      value: '_gte',
    },
    {
      name: '<=',
      value: '_lte',
    },
    {
      name: 'like',
      value: '_like',
      defaultValue: '%%',
    },
    {
      name: 'not like',
      value: '_nlike',
      defaultValue: '%%',
    },
    {
      name: 'like (case-insensitive)',
      value: '_ilike',
      defaultValue: '%%',
    },
    {
      name: 'not like (case-insensitive)',
      value: '_nilike',
      defaultValue: '%%',
    },
    {
      name: 'similar',
      value: '_similar',
    },
    {
      name: 'not similar',
      value: '_nsimilar',
    },
    {
      name: '~',
      value: '_regex',
    },
    {
      name: '~*',
      value: '_iregex',
    },
    {
      name: '!~',
      value: '_nregex',
    },
    {
      name: '!~*',
      value: '_niregex',
    },
  ];
  return graphqlOps;
};
