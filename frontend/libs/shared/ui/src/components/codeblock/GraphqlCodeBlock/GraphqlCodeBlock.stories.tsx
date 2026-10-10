import { Meta, StoryObj } from '@storybook/react-webpack5';
import { GraphqlCodeBlock } from './GraphqlCodeBlock';

export default {
  title: 'components/codeblock/GraphqlCodeBlock',
  component: GraphqlCodeBlock,
} as Meta<typeof GraphqlCodeBlock>;

type Story = StoryObj<typeof GraphqlCodeBlock>;

const sampleQuery = `query GetUser($id: uuid!) {
  users_by_pk(id: $id) {
    id
    name
    email
    posts {
      id
      title
    }
  }
}`;

export const Default: Story = {
  args: {
    text: sampleQuery,
  },
};

export const LongContentScrollable: Story = {
  args: {
    text: Array.from(
      { length: 15 },
      (_, i) =>
        `query GetItem${i} {\n  item(id: ${i}) {\n    id\n    name\n  }\n}`,
    ).join('\n\n'),
  },
};

export const NoScrollLimit: Story = {
  args: {
    text: sampleQuery,
    scrollable: false,
  },
};

export const HideCopyButton: Story = {
  args: {
    text: sampleQuery,
    hideCopyButton: true,
  },
};
