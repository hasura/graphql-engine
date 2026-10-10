import { StoryObj, Meta } from '@storybook/react-webpack5';

import { LegacyBadge } from './LegacyBadge';

export default {
  title: 'components/Badge/LegacyBadge',
  parameters: {
    docs: {
      description: {
        component: `⚠️ Legacy badge component predating [Badge](../Badge). Renders a small colored pill for a fixed set of known \`type\`s (returns \`null\` for unknown types).`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    (Story) => (
      <div className="p-4 flex gap-2 flex-wrap items-center">{Story()}</div>
    ),
  ],
  component: LegacyBadge,
} as Meta<typeof LegacyBadge>;

export const ApiPlayground: StoryObj<typeof LegacyBadge> = {
  args: {
    type: 'update',
  },

  name: '⚙️ API',
};

export const AllTypes: StoryObj<typeof LegacyBadge> = {
  render: () => (
    <>
      <LegacyBadge type="version update" />
      <LegacyBadge type="community" />
      <LegacyBadge type="beta update" />
      <LegacyBadge type="update" />
      <LegacyBadge type="feature" />
      <LegacyBadge type="security" />
      <LegacyBadge type="error" />
      <LegacyBadge type="experimental" />
    </>
  ),

  name: '🎭 Variant - All types',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const RestApiTypes: StoryObj<typeof LegacyBadge> = {
  render: () => (
    <>
      <LegacyBadge type="rest-GET" />
      <LegacyBadge type="rest-PUT" />
      <LegacyBadge type="rest-POST" />
      <LegacyBadge type="rest-PATCH" />
      <LegacyBadge type="rest-DELETE" />
    </>
  ),

  name: '🎭 Variant - REST API methods',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateUnknownType: StoryObj<typeof LegacyBadge> = {
  render: () => <LegacyBadge type="not-a-known-type" />,

  name: '🔁 State - Unknown type (renders nothing)',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
