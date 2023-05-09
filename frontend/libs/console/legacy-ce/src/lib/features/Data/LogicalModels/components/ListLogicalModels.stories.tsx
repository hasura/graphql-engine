import { Meta, StoryObj } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../AddNativeQuery/mocks/handlers';
import { ListLogicalModels } from './ListLogicalModels';

export default {
  component: ListLogicalModels,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ postgres: { models: true, queries: true } }),
  },
  argTypes: {
    dataSourceName: { defaultValue: 'postgres' },
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} satisfies Meta<typeof ListLogicalModels>;

export const Basic: StoryObj<typeof ListLogicalModels> = {
  args: {
    dataSourceName: 'postgres',
  },
};
