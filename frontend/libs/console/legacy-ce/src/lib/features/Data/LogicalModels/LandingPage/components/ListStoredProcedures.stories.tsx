import { Meta, StoryObj } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { handlers } from '../../LogicalModelWidget/mocks/handlers';
import { ListStoredProcedures } from './ListStoredProcedures';

export default {
  component: ListStoredProcedures,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof ListStoredProcedures>;

export const Basic: StoryObj<typeof ListStoredProcedures> = {
  render: (args) => {
    return <ListStoredProcedures />;
  },

  parameters: {
    msw: handlers['200'],
  },
};
