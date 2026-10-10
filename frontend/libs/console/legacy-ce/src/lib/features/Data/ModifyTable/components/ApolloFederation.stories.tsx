import { Meta, StoryObj } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { ApolloFederation } from './ApolloFederation';
import { MetadataTable, Source } from '@hasura/shared/types';

type Story = StoryObj<typeof ApolloFederation>;

export default {
  component: ApolloFederation,
  decorators: [ReactQueryDecorator()],
  parameters: {
    layout: 'fullscreen',
  },
} satisfies Meta<typeof ApolloFederation>;

export const Basic: Story = {
  render: () => (
    <div className="p-5">
      <ApolloFederation
        source={{ name: 'chinook_12345', kind: 'postgres' } as Source}
        table={{ table: { name: 'Album', schema: 'public' } } as MetadataTable}
      />
    </div>
  ),
};
