import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../AddNativeQuery/mocks/handlers';
import { ListNativeQueries } from './ListNativeQueries';

export default {
  component: ListNativeQueries,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ postgres: { models: true, queries: true } }),
  },
  argTypes: {
    dataSourceName: { defaultValue: 'postgres' },
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} as ComponentMeta<typeof ListNativeQueries>;

export const Basic: ComponentStory<typeof ListNativeQueries> = args => {
  return <ListNativeQueries {...args} />;
};
