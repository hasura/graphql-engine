import { Operator } from '../../types';

export const getSupportedOperators = async () => {
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
      name: 'nin',
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
  ];
  return graphqlOps;
};
