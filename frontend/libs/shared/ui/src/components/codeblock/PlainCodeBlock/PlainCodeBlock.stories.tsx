import { Meta, StoryObj } from '@storybook/react-webpack5';
import { PlainCodeBlock } from './PlainCodeBlock';

export default {
  title: 'components/codeblock/PlainCodeBlock',
  component: PlainCodeBlock,
} as Meta<typeof PlainCodeBlock>;

type Story = StoryObj<typeof PlainCodeBlock>;

const sampleText = `Seq Scan on users  (cost=0.00..1.05 rows=5 width=40)
  Filter: (status = 'active'::text)
Planning Time: 0.082 ms
Execution Time: 0.021 ms`;

export const Default: Story = {
  args: {
    value: sampleText,
  },
};

export const LongContentScrollable: Story = {
  args: {
    value: Array.from(
      { length: 40 },
      (_, i) => `line ${i}: some log output`,
    ).join('\n'),
  },
};

export const NoScrollLimit: Story = {
  args: {
    value: sampleText,
    scrollable: false,
  },
};

export const HideCopyButton: Story = {
  args: {
    value: sampleText,
    hideCopyButton: true,
  },
};
