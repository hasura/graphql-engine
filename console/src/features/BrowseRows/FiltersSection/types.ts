export type FiltersAndSortFormValues = {
  filter: {
    column: string;
    operator: string;
    value: string;
  }[];
  sort: {
    column: string;
    order: 'asc' | 'desc' | '--';
  }[];
};

export const defaultColumn = '-- column --';
export const defaultOperator = '$eq';
export const defaultOrder = '--';

type ColumnName = string;

type OperatorType = string; // TODO: find the list of possible operators
type OperatorCondition = Record<
  OperatorType,
  string | number | number[] | string[] | boolean
>;

export type WhereCondition = Record<ColumnName, OperatorCondition>;

export type OrderCondition = {
  column: string;
  type: 'desc' | 'asc' | '--';
  nulls: 'last';
};

export type UserQuery = {
  where: Record<'$and', WhereCondition[]>;
  order_by: OrderCondition[];
};
