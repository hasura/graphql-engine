import { ReactNode } from 'react';
import { Skeleton } from '@radix-ui/themes';
import {
  Table,
  TableHeaderProps,
  TableProps,
  Table as ThemeTable,
  BodyProps as ThemeTableBodyProps,
} from '../Table';

type HeaderProps = TableHeaderProps & {
  columns: Array<ReactNode>;
};

const Header = ({ columns, ...rest }: HeaderProps) => {
  return (
    <ThemeTable.Header {...rest}>
      <ThemeTable.Row>
        {columns.map((column, i) => (
          <ThemeTable.ColumnHeaderCell key={i}>
            {column}
          </ThemeTable.ColumnHeaderCell>
        ))}
      </ThemeTable.Row>
    </ThemeTable.Header>
  );
};

type BodyProps = ThemeTableBodyProps & {
  data: ReactNode[][];
};

const Body = ({ data, ...rest }: BodyProps) => {
  const maxCells = data.reduce((acc, cells) => Math.max(acc, cells.length), 0);

  return (
    <ThemeTable.Body {...rest}>
      {data.map((row, rowIndex) => {
        const colSpan = Math.floor(maxCells / row.length);

        return (
          <ThemeTable.Row key={rowIndex}>
            {row.map((cell, cellIndex) => {
              return (
                <ThemeTable.Cell colSpan={colSpan} key={cellIndex}>
                  {cell}
                </ThemeTable.Cell>
              );
            })}
          </ThemeTable.Row>
        );
      })}
    </ThemeTable.Body>
  );
};

type CardedTableProps = Omit<TableProps, 'children'> & {
  columns: Array<ReactNode>;
  data: ReactNode[][];
  /**
   * The header orientation of table.
   */
  orientation?: 'vertical' | 'horizontal';
  /**
   * Shows skeleton rows instead of `data`.
   */
  loading?: boolean;
  /**
   * The number of skeleton rows shown while loading.
   */
  loadingRowCount?: number;
  options?: {
    header?: Omit<TableHeaderProps, 'children'>;
    body?: Omit<ThemeTableBodyProps, 'children'>;
  };
};

const getLoadingData = (columnCount: number, rowCount: number) =>
  Array.from({ length: rowCount }, () =>
    Array.from({ length: columnCount }, (_, i) => (
      <Skeleton key={i} height="16px" data-testid="carded-table-loading-cell" />
    )),
  );

export const CardedTable = ({
  columns,
  data: rows,
  options,
  variant = 'surface',
  orientation,
  loading,
  loadingRowCount = 5,
  ...rest
}: CardedTableProps) => {
  const data = loading ? getLoadingData(columns.length, loadingRowCount) : rows;

  if (orientation == 'horizontal') {
    return (
      <Table.Root variant={variant} {...rest}>
        <ThemeTable.Body {...options?.body}>
          {columns.map((column, columnIndex) => {
            return (
              <ThemeTable.Row key={columnIndex}>
                <ThemeTable.ColumnHeaderCell key={`column-${columnIndex}`}>
                  {column}
                </ThemeTable.ColumnHeaderCell>
                {data.map((row) => (
                  <ThemeTable.Cell key={`column-${columnIndex}`}>
                    {row[columnIndex]}
                  </ThemeTable.Cell>
                ))}
              </ThemeTable.Row>
            );
          })}
        </ThemeTable.Body>
      </Table.Root>
    );
  }

  return (
    <Table.Root variant={variant} {...rest}>
      <Header {...options?.header} columns={columns} />
      <Body {...options?.body} data={data} />
    </Table.Root>
  );
};

CardedTable.Header = Header;
CardedTable.Body = Body;
