import { OrderBy } from '../utils/v1QueryUtils';

// TODO rethink design

export type Operator =
  | '$eq'
  | '$ne'
  | '$in'
  | '$nin'
  | '$gt'
  | '$lt'
  | '$gte'
  | '$lte'
  | '$like'
  | '$nlike'
  | '$ilike'
  | '$nilike'
  | '$similar'
  | '$nsimilar'
  | '';

export type OperatorDef = {
  alias: string;
  operator: Operator;
  name: string;
  default?: string;
};

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
];

export const getOperatorDefaultValue = (op: Operator) => {
  const operator = allOperators.find(o => o.operator === op);
  return operator ? operator.default : '';
};

export type ValueFilter = {
  kind: 'value';
  key: string;
  operator: Operator;
  value: string;
};
export type RelationshipFilter = {
  kind: 'relationship';
  key: string;
  value: Filter;
};
export type OperatorFilter = {
  kind: 'operator';
  key: '$or' | '$and' | '$not';
  value: Filter[];
};

export type Filter = ValueFilter | RelationshipFilter | OperatorFilter;

export const parseFilter = (f: Filter): any => {
  switch (f.kind) {
    case 'value':
      return {
        [f.key]: {
          [f.operator]: f.value,
        },
      };
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

export type FilterState = {
  filters: ValueFilter[];
  sorts: OrderBy[];
  limit: number;
  offset: number;
};

export type SetSorts = (s: OrderBy[]) => void;
export type SetFilters = (s: ValueFilter[]) => void;
export type SetFilterState = {
  sorts: SetSorts;
  filters: SetFilters;
  offset: (o: number) => void;
  limit: (l: number) => void;
};
export type RunQuery = (offset?: number, limit?: number) => void;

export type FilterRenderProp = (
  rows: any[],
  count: number | undefined,
  state: FilterState,
  setState: SetFilterState,
  runQuery: RunQuery
) => JSX.Element;
