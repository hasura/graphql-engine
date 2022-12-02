export type validOperators = string;

export type WhereClause = Record<
  validOperators,
  Record<string, string | number | boolean>
>[];

export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';
export type OrderBy = {
  column: string;
  type: OrderByType;
  nulls?: OrderByNulls;
};

export type GraphQLType = {
  name: string;
  kind: string;
  ofType: GraphQLType;
};
