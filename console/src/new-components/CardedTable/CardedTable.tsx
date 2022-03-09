import React, { ReactNode } from 'react';

interface ChildrenProps {
  children?: ReactNode;
}

const Table = ({ children }: ChildrenProps) => {
  return (
    <div className="overflow-x-auto border border-gray-300 rounded mb-md">
      <table className="min-w-full divide-y divide-gray-200 text-left">
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
        {columns.map(column => (
          <TableHeadCell>{column}</TableHeadCell>
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

const TableBodyRow = ({ children }: ChildrenProps) => {
  return <tr className="group">{children}</tr>;
};

const TableBodyCell = ({ children }: ChildrenProps) => {
  return (
    <td className="px-sm py-xs whitespace-nowrap text-muted">{children}</td>
  );
};

const TableBodyActionCell = ({ children }: ChildrenProps) => {
  return (
    <td className="px-sm py-xs whitespace-nowrap text-right font-semibold opacity-0 group-hover:opacity-100">
      {children}
    </td>
  );
};

interface BodyProps {
  data: ReactNode[][];
  showActionCell?: boolean;
}

const Body = ({ data, showActionCell = false }: BodyProps) => {
  return (
    <TableBody>
      {data.map(row => {
        return (
          <TableBodyRow>
            {row.map((cell, index) => {
              if (showActionCell && index + 1 === row.length) {
                return <TableBodyActionCell>{cell}</TableBodyActionCell>;
              }
              return <TableBodyCell>{cell}</TableBodyCell>;
            })}
          </TableBodyRow>
        );
      })}
    </TableBody>
  );
};

type CardedTableProps = HeaderProps & BodyProps;

export const CardedTable = (props: CardedTableProps) => {
  return (
    <Table>
      <Header columns={props.columns} />
      <Body data={props.data} showActionCell={props.showActionCell} />
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
