import React from 'react';
import { FaDatabase, FaTable } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { Breadcrumbs } from '@hasura/shared/ui';
import { dataRoutes, getTableDisplayName } from '@hasura/shared/utils';
import { Table } from '@hasura/shared/types';

export const TableBreadcrumbs: React.FC<{
  dataSourceName: string;
  table: Table | undefined;
}> = ({ dataSourceName, table }) => (
  <Flex className="mb-4">
    <Breadcrumbs
      items={[
        {
          title: 'Data',
          url: dataRoutes.manageDatabase,
        },
        {
          title: dataSourceName,
          icon: <FaDatabase />,
          url: dataRoutes.manageDatabaseSource(dataSourceName),
        },
        ...(table
          ? [
              {
                title: getTableDisplayName(table, '', ' / '),
                icon: <FaTable />,
              },
            ]
          : []),
      ]}
    />
  </Flex>
);
