import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ListNativeQueries } from './ListNativeQueries';
import { buildMetadata } from '../../mocks/metadata';
import { MetadataSelectors } from '@hasura/metadata/helpers';

export default {
  component: ListNativeQueries,
  argTypes: {
    dataSourceName: { defaultValue: 'postgres' },
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} as Meta<typeof ListNativeQueries>;

const data = MetadataSelectors.extractModelsAndQueriesFromMetadata(
  buildMetadata({
    postgres: { models: true, queries: true },
    mssql: { models: true, queries: true },
  }),
);

export const Basic: StoryObj<typeof ListNativeQueries> = {
  render: (args) => {
    return <ListNativeQueries {...args} nativeQueries={data.queries} />;
  },
};
