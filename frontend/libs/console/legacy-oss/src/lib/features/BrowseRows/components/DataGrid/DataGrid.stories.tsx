import React, { useState } from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { DataGrid } from './DataGrid';
import { UseRowsData, DataGridRowSortProps } from './types';

export default {
  title: 'Browse Rows/Components/DataGrid',
  component: DataGrid,
} as ComponentMeta<typeof DataGrid>;

const ascRows = [
  {
    id: 1,
    name: 'Mr John',
  },
  {
    id: 2,
    name: 'Mr Doe',
  },
];

const descRows = [
  {
    id: 2,
    name: 'Mr Doe',
  },
  {
    id: 1,
    name: 'Mr John',
  },
];

const data: UseRowsData = {
  rows: ascRows,
  columns: ['id', 'name'],
  orderBy: null,
};

export const Primary: ComponentStory<typeof DataGrid> = () => {
  const [orderBy, setOrderBy] = useState<DataGridRowSortProps>(null);

  const onColumnSortClick = (column: string) => {
    // If  user switches sorting to another column we start with ascending order
    if (column !== orderBy?.column) {
      return setOrderBy({ column, type: 'asc', nulls: 'last' });
    }
    if (orderBy === null) {
      return setOrderBy({ column, type: 'asc', nulls: 'last' });
    } else if (orderBy?.type === 'asc') {
      return setOrderBy({ column, type: 'desc', nulls: 'last' });
    } else if (orderBy?.type === 'desc') {
      setOrderBy(null);
    }
  };

  return (
    <DataGrid
      data={{
        ...data,
        orderBy,
        rows: orderBy?.type === 'desc' ? descRows : ascRows,
      }}
      onColumnSortClick={onColumnSortClick}
      isLoading={false}
    />
  );
};
