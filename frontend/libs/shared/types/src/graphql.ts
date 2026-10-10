export type GraphiqlMode = 'graphql' | 'relay';

export type Where =
  | {
      _and: WhereClause[];
    }
  | {
      _or: WhereClause[];
    }
  | WhereClause;

export type WhereClause = Record<
  string,
  Record<string, string | number | boolean | string[] | number[] | boolean[]>
>;

/**
 * The common input body of a GraphQL request.
 */
export type GraphQLRequestInput = {
  query: string;
  variables?: Record<string, any> | null;
  operationName?: string | null;
};

/**
 * The common data header type for header forms of Graphiql and REST.
 */
export type DataHeader = {
  key: string;
  value: string;
  selected: boolean;
  isDisabled: boolean;
};

export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';

export type OrderBy = {
  column: string;
  type: OrderByType;
  nulls?: OrderByNulls | null;
};
