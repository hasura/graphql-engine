import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { handlers } from '../mocks';
import { ListLogicalModels } from './ListLogicalModels';

export default {
  component: ListLogicalModels,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
  argTypes: {
    dataSourceName: { defaultValue: 'postgres' },
    onEditClick: { action: 'onEdit' },
    onRemoveClick: { action: 'onRemove' },
  },
} as ComponentMeta<typeof ListLogicalModels>;

export const Basic: ComponentStory<typeof ListLogicalModels> = args => {
  return <ListLogicalModels {...args} />;
};
