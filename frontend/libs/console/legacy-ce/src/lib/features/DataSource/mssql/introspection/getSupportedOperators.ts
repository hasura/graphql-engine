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
  ];
  return graphqlOps;
};
