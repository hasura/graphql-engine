export type ColumnName = string;
export type RowValues = {
  // Can we different kinds of HTML elements, but these are the used only  values
  valueNode: { value?: string; props?: { value: string } } | null;
  nullNode: HTMLInputElement | null;
  defaultNode: HTMLInputElement | null;
  radioNode: HTMLInputElement | null;
};
export type Values = Record<ColumnName, RowValues>;
