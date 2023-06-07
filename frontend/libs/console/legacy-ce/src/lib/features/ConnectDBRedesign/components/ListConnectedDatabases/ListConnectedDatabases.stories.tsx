import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { StoryObj, Meta } from '@storybook/react';
import { handlers } from '../../mocks/handlers.mock';
import { ListConnectedDatabases } from './ListConnectedDatabases';

export default {
  component: ListConnectedDatabases,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof ListConnectedDatabases>;

export const Basic: StoryObj<typeof ListConnectedDatabases> = {
  render: () => <ListConnectedDatabases />,

  parameters: {
    msw: handlers(),
  },
};
