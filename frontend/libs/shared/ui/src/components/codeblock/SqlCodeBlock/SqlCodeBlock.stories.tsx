import { Meta, StoryObj } from '@storybook/react-webpack5';
import { SqlCodeBlock } from './SqlCodeBlock';

export default {
  title: 'components/codeblock/SqlCodeBlock',
  component: SqlCodeBlock,
} as Meta<typeof SqlCodeBlock>;

type Story = StoryObj<typeof SqlCodeBlock>;

const sampleQuery =
  "select id, name, email from users where status = 'active' and created_at > now() - interval '7 days' order by created_at desc limit 10;";

export const Default: Story = {
  args: {
    text: sampleQuery,
  },
};

export const LongContentScrollable: Story = {
  args: {
    text: Array.from(
      { length: 20 },
      (_, i) =>
        `select * from table_${i} where id = ${i} and status = 'active';`,
    ).join('\n'),
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
