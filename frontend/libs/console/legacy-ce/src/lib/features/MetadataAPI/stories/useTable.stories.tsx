import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { useTables } from '../hooks/useMetadataTables';

function FetchTables({ database }: { database: string }) {
  const query = useTables(database);
  return (
    <div>
      {query.isSuccess ? <ReactJson src={query.data} /> : 'no response'}

      {query.isError ? <ReactJson src={query.error} /> : null}
    </div>
  );
}

export const FetchTableColumns: ComponentStory<typeof FetchTables> = args => {
  return <FetchTables {...args} />;
};

FetchTableColumns.args = {
  database: 'default',
};

export default {
  title: 'hooks/Table Queries/Fetch Tables',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as ComponentMeta<typeof FetchTables>;
