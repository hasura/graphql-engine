import { Meta, StoryObj } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ApolloFederation } from './ApolloFederation';
import { ReduxDecorator } from '../../../../storybook/decorators';

type Story = StoryObj<typeof ApolloFederation>;

export default {
  component: ApolloFederation,
  decorators: [
    ReactQueryDecorator(),
    ReduxDecorator({
      tables: {
        dataHeaders: {
          'x-hasura-admin-secret': 'myadminsecretkey',
        } as any,
      },
    }),
  ],
  parameters: {
    layout: 'fullscreen',
  },
} satisfies Meta<typeof ApolloFederation>;

export const Basic: Story = {
  render: () => (
    <div className="p-5">
      <ApolloFederation
        dataSourceName={'chinook_12345'}
        table={{ name: 'Album', schema: 'public' }}
      />
    </div>
  ),
};
