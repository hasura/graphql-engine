import React, { useState } from 'react';
import { useRows } from '@/components/Services/Data/TableBrowseRows/Hooks';
import { Feature } from '@/features/DataSource';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ColumnSort } from '@tanstack/react-table';
import { ReactTableWrapper } from './ReactTableWrapper';
import { transformToOrderByClause } from '../utils';
import { handlers } from '../../../__mocks__/handlers.mock';

export default {
  title: 'Browse Rows/React-Table Wrapper 🧬',
  component: ReactTableWrapper,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers('http://localhost:8080'),
  },
} as ComponentMeta<typeof ReactTableWrapper>;

export const Primary: ComponentStory<typeof ReactTableWrapper> = () => {
  const [sorting, setSorting] = useState<ColumnSort[]>([]);
  const { data: rows } = useRows({
    dataSourceName: 'sqlite_test',
    table: ['Album'],
    options: {
      order_by: transformToOrderByClause(sorting),
    },
  });

  if (!rows) return <>Loading...</>;

  if (rows === Feature.NotImplemented) return <>Not implemented</>;

  return (
    <ReactTableWrapper
      rows={rows}
      sort={{
        sorting,
        setSorting,
      }}
    />
  );
};
