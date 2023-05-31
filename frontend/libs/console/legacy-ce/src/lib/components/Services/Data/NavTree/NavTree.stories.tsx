import type { Meta, StoryObj } from '@storybook/react';
import { NavTree } from './NavTree';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from './mocks/handlers';

export default {
  component: NavTree,
  decorators: [ReactQueryDecorator()],
} satisfies Meta<typeof NavTree>;

type Story = StoryObj<typeof NavTree>;

export const Primary: Story = {
  render: () => (
    <NavTree
      defaultSelection={{
        dataSourceName: 'chinook',
        table: { name: 'Album', schema: 'public' },
      }}
    />
  ),
  parameters: {
    msw: handlers(),
  },
};
