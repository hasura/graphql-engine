import { Table } from '../../../hasura-metadata-types';
import { Link } from '../../TrackResources/components/parts/Link';
import { getQualifiedTable } from '../utils';
import { FaTable } from 'react-icons/fa';

export const TableDisplayName = ({
  dataSourceName,
  table,
  onClick,
}: {
  onClick?: () => void;
  dataSourceName?: string;
  table: Table;
}) => {
  if (!table) return null;

  const tableName = getQualifiedTable(table);
  const content = () => (
    <>
      <FaTable className="text-sm text-muted mr-xs" />
      {dataSourceName ? (
        <>
          {dataSourceName} / {tableName.join(' / ')}
        </>
      ) : (
        <>{tableName.join(' / ')}</>
      )}
    </>
  );

  return onClick ? (
    <Link onClick={onClick}>{content()}</Link>
  ) : (
    <div>{content()}</div>
  );
};
