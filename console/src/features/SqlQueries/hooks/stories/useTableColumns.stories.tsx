import { QualifiedTable } from '@/metadata/types';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import ReactJson from 'react-json-view';
import { useTableColumns } from '../useTableColumns';

function FetchTableColumnsComponent({
  database,
  table,
}: {
  database: string;
  table: QualifiedTable;
}) {
  const query = useTableColumns(database, table);
  return (
    <div>
      {query.isSuccess ? <ReactJson src={query.data} /> : 'no response'}

      {query.isError ? <ReactJson src={query.error} /> : null}
    </div>
  );
}

export const FetchTableColumns: ComponentStory<
  typeof FetchTableColumnsComponent
> = args => {
  return <FetchTableColumnsComponent {...args} />;
};

FetchTableColumns.args = {
  database: 'default',
  table: {
    name: 'test',
    schema: 'public',
  },
};

export default {
  title: 'hooks/Table Queries/Fetch Columns',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof FetchTableColumnsComponent>;
