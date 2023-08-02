import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { StoryObj, Meta } from '@storybook/react';
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

export const FetchTableColumns: StoryObj<typeof FetchTables> = {
  render: args => {
    return <FetchTables {...args} />;
  },

  args: {
    database: 'default',
  },
};

export default {
  title: 'hooks/Table Queries/Fetch Tables',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
} as Meta<typeof FetchTables>;
