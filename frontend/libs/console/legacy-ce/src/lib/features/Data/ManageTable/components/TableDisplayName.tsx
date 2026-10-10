import { Table } from '@hasura/shared/types';
import { getQualifiedTable } from '../utils';
import { FaTable } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { To } from 'react-router';
import { RelativeLink } from '@hasura/shared/ui';

export const TableDisplayName = ({
  dataSourceName,
  table,
  to,
}: {
  to?: To;
  dataSourceName?: string;
  table: Table;
}) => {
  if (!table) return null;

  const tableName = getQualifiedTable(table);
  const content = () => (
    <Flex align="center" gap="1">
      <FaTable />
      {dataSourceName ? (
        <>
          {dataSourceName} / {tableName.join(' / ')}
        </>
      ) : (
        <>{tableName.join(' / ')}</>
      )}
    </Flex>
  );

  return to ? <RelativeLink to={to}>{content()}</RelativeLink> : content();
};
