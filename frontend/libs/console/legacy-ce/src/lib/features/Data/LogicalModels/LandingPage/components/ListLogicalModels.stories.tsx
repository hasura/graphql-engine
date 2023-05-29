import { StoryObj, Meta } from '@storybook/react';
import { buildMetadata } from '../../mocks/metadata';
import { extractModelsAndQueriesFromMetadata } from '../../utils';
import { ListLogicalModels } from './ListLogicalModels';

export default {
  component: ListLogicalModels,
  argTypes: {
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} as Meta<typeof ListLogicalModels>;

const data = extractModelsAndQueriesFromMetadata(
  buildMetadata({
    postgres: { models: true, queries: true },
    mssql: { models: true, queries: true },
  })
);
export const Basic: StoryObj<typeof ListLogicalModels> = {
  render: args => {
    return <ListLogicalModels {...args} logicalModels={data.models} />;
  },
};
export const Loading: StoryObj<typeof ListLogicalModels> = {
  render: args => {
    return <ListLogicalModels {...args} isLoading />;
  },
};
