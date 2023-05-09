import { Meta, StoryObj } from '@storybook/react';
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
} satisfies Meta<typeof ListNativeQueries>;

export const Basic: StoryObj<typeof ListNativeQueries> = {
  args: {
    dataSourceName: 'postgres',
  },
};
