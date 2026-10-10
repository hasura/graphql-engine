import { Table as ThemeTable } from '@radix-ui/themes';

export type TableProps = ThemeTable.RootProps;
export type BodyProps = ThemeTable.BodyProps;
export type RowProps = ThemeTable.RowProps;
export type CellProps = ThemeTable.CellProps;
export type ColumnHeaderCellProps = ThemeTable.ColumnHeaderCellProps;
export type TableHeaderProps = ThemeTable.HeaderProps;
export type RowHeaderCellProps = ThemeTable.RowHeaderCellProps;

export const Table = {
  Root: ThemeTable.Root,
  Header: ThemeTable.Header,
  Body: ThemeTable.Body,
  Row: ThemeTable.Row,
  Cell: ThemeTable.Cell,
  ColumnHeaderCell: ThemeTable.ColumnHeaderCell,
  RowHeaderCell: ThemeTable.RowHeaderCell,
};
