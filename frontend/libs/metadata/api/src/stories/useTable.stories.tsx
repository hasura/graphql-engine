// import { ReactQueryDecorator } from '@hasura/shared/testing';
import { JsonCodeBlock } from '@hasura/shared/ui';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { handlers } from './mocks/handlers.mock';
import { useMetadata } from '../hooks';
import { Metadata } from '@hasura/shared/types';

const getTables = (database: string) => (m: Metadata) =>
  m.metadata?.sources?.find((source) => source.name === database)?.tables ?? [];

function FetchTables({ database }: { database: string }) {
  const query = useMetadata(getTables(database));
  return (
    <div>
      {query.isSuccess ? <JsonCodeBlock value={query.data} /> : 'no response'}

      {query.isError ? <JsonCodeBlock value={query.error} /> : null}
    </div>
  );
}

export const FetchTableColumns: StoryObj<typeof FetchTables> = {
  render: (args) => {
    return <FetchTables {...args} />;
  },

  args: {
    database: 'default',
  },
};

export default {
  title: 'hooks/Table Queries/Fetch Tables',
  decorators: [
    // ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof FetchTables>;
