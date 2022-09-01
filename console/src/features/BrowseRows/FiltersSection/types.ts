export type FormValues = {
  filter: {
    column: string;
    operator: string;
    value: string;
  }[];
  sort: {
    column: string;
    order: string;
  }[];
};

export const defaultColumn = '-- column --';
export const defaultOperator = '$eq';
export const defaultOrder = '--';
