import { StoryObj, Meta } from '@storybook/react-webpack5';
import { buildMetadata } from '../../mocks/metadata';
import { ListLogicalModels } from './ListLogicalModels';
import { MetadataSelectors } from '@hasura/metadata/helpers';

export default {
  component: ListLogicalModels,
  argTypes: {
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} as Meta<typeof ListLogicalModels>;

const data = MetadataSelectors.extractModelsAndQueriesFromMetadata(
  buildMetadata({
    postgres: { models: true, queries: true },
    mssql: { models: true, queries: true },
  }),
);
export const Basic: StoryObj<typeof ListLogicalModels> = {
  render: (args) => {
    return <ListLogicalModels {...args} logicalModels={data.models} />;
  },
};
