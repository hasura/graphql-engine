import { Table } from '../../../hasura-metadata-types';
import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { useTableColumns } from '../../hooks';

type WidgetProps = {
  table: Table;
  dataSourceName: string;
};

export const Widget = (props: WidgetProps) => {
  const { data: columnInfo, isLoading } = useTableColumns({ ...props });

  if (isLoading)
    return (
      <div className="my-4">
        <Skeleton height={30} count={8} className="my-2" />
      </div>
    );

  if (!columnInfo?.columns.length)
    return <div>Columns could not be retrieved for the table</div>;

  return <div>FEATURE NOT IMPLEMENTED</div>;
};
