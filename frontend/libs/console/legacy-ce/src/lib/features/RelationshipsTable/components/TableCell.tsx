import React from 'react';
import { FaTable, FaColumns } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

const ColumnCell = ({ columnName }: { columnName: React.ReactNode }) => (
  <>
    <FaColumns
      className="fill-current text-sm text-muted mr-1"
      title="Column"
    />
    <span className="mr-2">{columnName}</span>
  </>
);

const TableCell = ({
  tableName,
  cols,
}: {
  tableName: React.ReactNode;
  cols?: React.ReactNode[];
}) => (
  <Flex align="center">
    <FaTable className="fill-current text-sm text-muted mr-1" title="Table" />
    {tableName}
    <span className="px-2">/</span>
    {cols
      ? cols.map((i, index) => <ColumnCell key={index} columnName={i} />)
      : null}
  </Flex>
);

export default TableCell;
