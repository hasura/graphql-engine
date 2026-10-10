import { StoryObj, Meta } from '@storybook/react-webpack5';

import { SanitizeTips } from './index';

export default {
  title: 'components/SanitizeTips',
  parameters: {
    docs: {
      description: {
        component: `Displays static tips explaining how GraphQL field names get sanitized.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
  component: SanitizeTips,
} as Meta<typeof SanitizeTips>;

export const ApiPlayground: StoryObj<typeof SanitizeTips> = {
  name: '⚙️ API',
};

export const Above: StoryObj<typeof SanitizeTips> = {
  render: () => (
    <>
      <SanitizeTips position="above" />
      <input
        type="text"
        placeholder="field name"
        className="block w-full h-input shadow-sm rounded border border-gray-300"
      />
    </>
  ),

  name: '🎭 Variant - Above (default)',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const Below: StoryObj<typeof SanitizeTips> = {
  render: () => (
    <>
      <input
        type="text"
        placeholder="field name"
        className="block w-full h-input shadow-sm rounded border border-gray-300"
      />
      <SanitizeTips position="below" />
    </>
  ),

  name: '🎭 Variant - Below',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
