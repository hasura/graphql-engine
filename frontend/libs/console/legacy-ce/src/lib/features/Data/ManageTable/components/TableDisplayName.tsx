import { Table } from '../../../hasura-metadata-types';
import { getQualifiedTable } from '../utils';
import { FaTable } from 'react-icons/fa';

export const TableDisplayName = ({
  dataSourceName,
  table,
}: {
  dataSourceName?: string;
  table: Table;
}) => {
  const tableName = getQualifiedTable(table);

  console.log(tableName);
  return (
    <div>
      <FaTable className="text-sm text-muted mr-xs" />
      {dataSourceName ? (
        <>
          {dataSourceName} / {tableName.join(' / ')}
        </>
      ) : (
        <>{tableName.join(' / ')}</>
      )}
    </div>
  );
};
