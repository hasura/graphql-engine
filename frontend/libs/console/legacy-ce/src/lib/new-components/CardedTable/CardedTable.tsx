import React, { CSSProperties, ReactNode } from 'react';
import clsx from 'clsx';

interface ChildrenProps {
  children?: ReactNode;

  className?: string;

  style?: CSSProperties;
}

const Table = ({ children, className, style, ...rest }: ChildrenProps) => {
  return (
    <div
      className={clsx(
        'overflow-x-auto border border-gray-300 rounded mb-md',
        className ?? ''
      )}
      style={style}
    >
      <table
        className="min-w-full divide-y divide-gray-200 text-left"
        {...rest}
      >
        {children}
      </table>
    </div>
  );
};

const TableHead = ({ children }: ChildrenProps) => {
  return <thead>{children}</thead>;
};

const TableHeadRow = ({ children }: ChildrenProps) => {
  return <tr>{children}</tr>;
};

const TableHeadCell = ({ children }: ChildrenProps) => {
  return (
    <th
      scope="col"
      className="bg-gray-50 px-sm py-xs text-sm font-semibold text-muted uppercase tracking-wider"
    >
      {children}
    </th>
  );
};

interface HeaderProps {
  columns: Array<ReactNode>;
}
const Header = ({ columns }: HeaderProps) => {
  return (
    <TableHead>
      <TableHeadRow>
        {columns.map((column, i) => (
          <TableHeadCell key={i}>{column}</TableHeadCell>
        ))}
      </TableHeadRow>
    </TableHead>
  );
};

const TableBody = ({ children }: ChildrenProps) => {
  return (
    <tbody className="bg-white divide-y divide-gray-200">{children}</tbody>
  );
};

const TableBodyRow = (props: React.ComponentProps<'tr'>) => {
  const { children, className, ...tableRowAttributes } = props;
  return (
    <tr className={clsx('group relative', className)} {...tableRowAttributes}>
      {children}
    </tr>
  );
};

const TableBodyCell = ({ children, ...cellAttributes }: ChildrenProps) => {
  return (
    <td
      // style={{ maxWidth: '20ch' }}
      className="px-sm py-xs overflow-hidden text-ellipsis"
      {...cellAttributes}
    >
      {children}
    </td>
  );
};

const TableBodyActionCell = ({ children }: ChildrenProps) => {
  return <td className="px-sm py-xs text-right font-semibold">{children}</td>;
};

interface BodyProps {
  data: ReactNode[][];
  showActionCell?: boolean;
  keyBuilder?: (cellIndex: number) => string;
  rowClassNames?: string[];
}

const Body = ({
  data,
  showActionCell = false,
  keyBuilder,
  rowClassNames,
}: BodyProps) => {
  return (
    <TableBody>
      {data.map((row, rowIndex) => {
        return (
          <TableBodyRow
            className={rowClassNames?.[rowIndex]}
            key={keyBuilder?.(rowIndex) ?? rowIndex}
            data-key={keyBuilder?.(rowIndex) ?? rowIndex}
          >
            {row.map((cell, cellIndex) => {
              if (showActionCell && cellIndex + 1 === row.length) {
                return (
                  <TableBodyActionCell key={cellIndex}>
                    {cell}
                  </TableBodyActionCell>
                );
              }
              return <TableBodyCell key={cellIndex}>{cell}</TableBodyCell>;
            })}
          </TableBodyRow>
        );
      })}
    </TableBody>
  );
};

type CardedTableProps = HeaderProps & BodyProps & React.ComponentProps<'table'>;

export const CardedTable = ({
  columns,
  data,
  showActionCell,
  keyBuilder,
  rowClassNames,
  ...rest
}: CardedTableProps) => {
  return (
    <Table {...rest}>
      <Header columns={columns} />
      <Body
        data={data}
        showActionCell={showActionCell}
        rowClassNames={rowClassNames}
        keyBuilder={keyBuilder}
      />
    </Table>
  );
};

CardedTable.Header = Header;
CardedTable.Body = Body;
CardedTable.Table = Table;
CardedTable.TableHead = TableHead;
CardedTable.TableHeadRow = TableHeadRow;
CardedTable.TableHeadCell = TableHeadCell;
CardedTable.TableBody = TableBody;
CardedTable.TableBodyRow = TableBodyRow;
CardedTable.TableBodyCell = TableBodyCell;
CardedTable.TableBodyActionCell = TableBodyActionCell;
