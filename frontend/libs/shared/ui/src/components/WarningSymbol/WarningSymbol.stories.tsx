import { StoryObj, Meta } from '@storybook/react-webpack5';

import { WarningSymbol } from './index';

export default {
  title: 'components/WarningSymbol',
  parameters: {
    docs: {
      description: {
        component: `An icon button showing a warning triangle with a tooltip explaining the warning.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
  component: WarningSymbol,
} as Meta<typeof WarningSymbol>;

export const ApiPlayground: StoryObj<typeof WarningSymbol> = {
  args: {
    tooltipText: 'This is a warning message',
  },

  name: '⚙️ API',
};

export const Basic: StoryObj<typeof WarningSymbol> = {
  render: () => <WarningSymbol tooltipText="This is a warning message" />,

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const TooltipPlacement: StoryObj<typeof WarningSymbol> = {
  render: () => (
    <div className="flex gap-8">
      <WarningSymbol tooltipText="Placed left" tooltipPlacement="left" />
      <WarningSymbol tooltipText="Placed right" tooltipPlacement="right" />
      <WarningSymbol tooltipText="Placed top" tooltipPlacement="top" />
      <WarningSymbol tooltipText="Placed bottom" tooltipPlacement="bottom" />
    </div>
  ),

  name: '🎭 Variant - Tooltip placement',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
