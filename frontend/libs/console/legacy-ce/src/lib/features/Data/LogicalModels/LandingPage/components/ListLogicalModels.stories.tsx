import { ComponentMeta, ComponentStory } from '@storybook/react';
import { buildMetadata } from '../../mocks/metadata';
import { extractModelsAndQueriesFromMetadata } from '../../utils';
import { ListLogicalModels } from './ListLogicalModels';

export default {
  component: ListLogicalModels,
  argTypes: {
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} as ComponentMeta<typeof ListLogicalModels>;

const data = extractModelsAndQueriesFromMetadata(
  buildMetadata({
    postgres: { models: true, queries: true },
    mssql: { models: true, queries: true },
  })
);
export const Basic: ComponentStory<typeof ListLogicalModels> = args => {
  return <ListLogicalModels {...args} logicalModels={data.models} />;
};
export const Loading: ComponentStory<typeof ListLogicalModels> = args => {
  return <ListLogicalModels {...args} isLoading />;
};
