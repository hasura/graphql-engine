import { StoryObj, Meta } from '@storybook/react';

import { ListNativeQueries } from './ListNativeQueries';
import { buildMetadata } from '../../mocks/metadata';
import { extractModelsAndQueriesFromMetadata } from '../../../../hasura-metadata-api/selectors';

export default {
  component: ListNativeQueries,
  argTypes: {
    dataSourceName: { defaultValue: 'postgres' },
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} as Meta<typeof ListNativeQueries>;

const data = extractModelsAndQueriesFromMetadata(
  buildMetadata({
    postgres: { models: true, queries: true },
    mssql: { models: true, queries: true },
  })
);

export const Basic: StoryObj<typeof ListNativeQueries> = {
  render: args => {
    return <ListNativeQueries {...args} nativeQueries={data.queries} />;
  },
};
