import React, { ReactText } from 'react';
import { FaTable, FaColumns } from 'react-icons/fa';

const ColumnCell = ({ columnName }: { columnName: ReactText }) => (
  <>
    <FaColumns className="fill-current text-sm text-muted mr-1" />
    {columnName}
  </>
);

const TableCell = ({
  tableName,
  cols,
}: {
  tableName: ReactText;
  cols?: ReactText[];
}) => (
  <>
    <div className="flex items-center">
      <FaTable className="fill-current text-sm text-muted mr-1" />
      {tableName}
      <span className="px-2">/</span>
      {cols ? cols.map(i => <ColumnCell columnName={i} />) : null}
    </div>
  </>
);

export default TableCell;
