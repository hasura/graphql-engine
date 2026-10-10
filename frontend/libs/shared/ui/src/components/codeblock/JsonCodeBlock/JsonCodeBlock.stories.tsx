import { Meta, StoryObj } from '@storybook/react-webpack5';
import { JsonCodeBlock } from './JsonCodeBlock';

export default {
  title: 'components/codeblock/JsonCodeBlock',
  component: JsonCodeBlock,
} as Meta<typeof JsonCodeBlock>;

type Story = StoryObj<typeof JsonCodeBlock>;

const sampleObject = {
  message: 'request failed for object #42',
  request: {
    proxy: null,
    secure: true,
    path: '/v1/graphql',
    responseTimeout: '60',
    method: 'POST',
    host: 'service-42.internal',
    requestVersion: '1',
    redirectCount: '0',
    port: '443',
  },
};

export const Default: Story = {
  args: {
    value: sampleObject,
  },
};

export const StringValue: Story = {
  args: {
    value: 'plain string message, shown as-is without JSON.stringify',
  },
};

export const LongContentScrollable: Story = {
  args: {
    value: {
      items: Array.from({ length: 50 }, (_, i) => ({
        id: i,
        name: `item-${i}`,
        nested: { a: i, b: i * 2, c: `value-${i}` },
      })),
    },
  },
};

export const NoScrollLimit: Story = {
  args: {
    value: sampleObject,
    scrollable: false,
  },
};

export const HideCopyButton: Story = {
  args: {
    value: sampleObject,
    hideCopyButton: true,
  },
};
