import { ReactQueryDecorator } from '@hasura/shared/testing';
import { StoryObj, Meta } from '@storybook/react-webpack5';
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
