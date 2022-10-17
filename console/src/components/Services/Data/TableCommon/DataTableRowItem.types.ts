export type ColumnName = string;
export type RowValues = {
  valueNode: HTMLSelectElement | HTMLInputElement | null;
  nullNode: HTMLInputElement | null;
  defaultNode: HTMLInputElement | null;
  radioNode: HTMLInputElement | null;
};
export type Values = Record<ColumnName, RowValues>;
