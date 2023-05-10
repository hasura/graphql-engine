import { ComponentMeta, ComponentStory } from '@storybook/react';

import { ListNativeQueries } from './ListNativeQueries';
import { buildMetadata } from '../../mocks/metadata';
import { extractModelsAndQueriesFromMetadata } from '../../utils';

export default {
  component: ListNativeQueries,
  argTypes: {
    dataSourceName: { defaultValue: 'postgres' },
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} as ComponentMeta<typeof ListNativeQueries>;

const data = extractModelsAndQueriesFromMetadata(
  buildMetadata({
    postgres: { models: true, queries: true },
    mssql: { models: true, queries: true },
  })
);

export const Basic: ComponentStory<typeof ListNativeQueries> = args => {
  return <ListNativeQueries {...args} nativeQueries={data.queries} />;
};
export const Loading: ComponentStory<typeof ListNativeQueries> = args => {
  return <ListNativeQueries {...args} isLoading />;
};
