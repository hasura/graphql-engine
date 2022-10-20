import { OrderBy } from '../utils/v1QueryUtils';
import { Nullable } from '../utils/tsUtils';

// Types for sorts
export type SetSorts = (s: OrderBy[]) => void;

// All supported postgres operators
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
  | '$regex'
  | '$iregex'
  | '$nregex'
  | '$niregex';

// Operator with names and aliases
export type OperatorDef = {
  alias: string;
  operator: Operator;
  name: string;
  default?: string;
};

/*
 * Value filter. Eg: { name: { $eq: "jondoe" } }
 */

export type ValueFilter = {
  kind: 'value';
  key: string;
  operator: Nullable<Operator>;
  value: string;
};
/*
 * Constructor for value filter
 */
export const makeValueFilter = (
  key: string,
  operator: Nullable<Operator>,
  value: string
): ValueFilter => ({ kind: 'value', key, operator, value });

/*
 * Relationship filter filter. Eg: { user { name: { $eq: "jondoe" } } }
 */

export type RelationshipFilter = {
  kind: 'relationship';
  key: string;
  value: Filter;
};
/*
 * Constructor for relationship filter
 */
export const makeRelationshipFilter = (
  key: string,
  value: Filter
): RelationshipFilter => ({ kind: 'relationship', key, value });

/*
 * Filter with logical gates
 * Eg: { $and: [ { title: { $eq: "My Title" } }, { author: { name: { $eq: "jon" }}} ]}
 */

type LogicGate = '$or' | '$and' | '$not';
export type OperatorFilter = {
  kind: 'operator';
  key: LogicGate;
  value: Filter[];
};

/*
 * Constructor for operation filter
 */
export const makeOperationFilter = (
  key: LogicGate,
  value: Filter[]
): OperatorFilter => ({ kind: 'operator', key, value });

/*
 * Filter for building the where clause
 * Filter could be a value filter, relationship filter, or a combination of filters
 */
export type Filter = ValueFilter | RelationshipFilter | OperatorFilter;

/*
 * Setter function for Filters
 */
export type SetValueFilters = (s: ValueFilter[]) => void;

/*
 * Local state for the filter query component
 */
export type FilterState = {
  filters: ValueFilter[];
  sorts: OrderBy[];
  limit: number;
  offset: number;
};
/*
 * Constructor for FilterState
 */
export const makeFilterState = (
  filters: ValueFilter[],
  sorts: OrderBy[],
  limit: number,
  offset: number
): FilterState => ({ filters, sorts, limit, offset });

/*
 * Local state setter for the filter query component
 */
export type SetFilterState = {
  sorts: SetSorts;
  filters: SetValueFilters;
  offset: (o: number) => void;
  limit: (l: number) => void;
};

export type RunQueryOptions = {
  offset?: number;
  limit?: number;
  sorts?: OrderBy[];
};

export type RunQuery = (options?: RunQueryOptions) => void;

export type FilterRenderProp = (
  rows: any[],
  count: number | undefined,
  state: FilterState,
  setState: SetFilterState,
  runQuery: RunQuery
) => React.ReactNode;
