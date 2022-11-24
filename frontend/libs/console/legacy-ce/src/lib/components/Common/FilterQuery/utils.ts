import { Operator, OperatorDef, Filter } from './types';

export const allOperators: OperatorDef[] = [
  { name: 'equals', operator: '$eq', alias: '_eq' },
  { name: 'not equals', operator: '$ne', alias: '_neq' },
  { name: 'in', operator: '$in', alias: '_in', default: '[]' },
  { name: 'not in', operator: '$nin', alias: '_nin', default: '[]' },
  { name: '>', operator: '$gt', alias: '_gt' },
  { name: '<', operator: '$lt', alias: '_lt' },
  { name: '>=', operator: '$gte', alias: '_gte' },
  { name: '<=', operator: '$lte', alias: '_lte' },
  { name: 'like', operator: '$like', alias: '_like', default: '%%' },
  {
    name: 'not like',
    operator: '$nlike',
    alias: '_nlike',
    default: '%%',
  },
  {
    name: 'like (case-insensitive)',
    operator: '$ilike',
    alias: '_ilike',
    default: '%%',
  },
  {
    name: 'not like (case-insensitive)',
    operator: '$nilike',
    alias: '_nilike',
    default: '%%',
  },
  { name: 'similar', operator: '$similar', alias: '_similar' },
  { name: 'not similar', operator: '$nsimilar', alias: '_nsimilar' },
  { name: '~', operator: '$regex', alias: '_regex' },
  {
    name: '~*',
    operator: '$iregex',
    alias: '_iregex',
  },
  {
    name: '!~',
    operator: '$nregex',
    alias: '_nregex',
  },
  {
    name: '!~*',
    operator: '$niregex',
    alias: '_niregex',
  },
];

export const getOperatorDefaultValue = (op: Operator) => {
  const operator = allOperators.find(o => o.operator === op);
  return operator ? operator.default : '';
};

export const parseFilter = (f: Filter): any => {
  switch (f.kind) {
    case 'value':
      return f.operator
        ? {
            [f.key]: {
              [f.operator]: f.value,
            },
          }
        : {};
      break;

    case 'relationship':
      return {
        [f.key]: parseFilter(f.value),
      };
      break;
    case 'operator':
      return {
        [f.key]: f.value.map(opFilter => parseFilter(opFilter)),
      };
      break;
    default:
      return parseFilter(f);
      break;
  }
};
