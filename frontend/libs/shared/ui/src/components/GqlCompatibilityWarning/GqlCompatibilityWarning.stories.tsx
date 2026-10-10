import { StoryObj, Meta } from '@storybook/react-webpack5';

import { GqlCompatibilityWarning } from './index';

export default {
  title: 'components/GqlCompatibilityWarning',
  parameters: {
    docs: {
      description: {
        component: `Shows a warning icon when an identifier (table, column, etc.) does not conform to the GraphQL naming standard. Renders nothing when the identifier is already GraphQL-compatible.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
  component: GqlCompatibilityWarning,
} as Meta<typeof GqlCompatibilityWarning>;

export const ApiPlayground: StoryObj<typeof GqlCompatibilityWarning> = {
  args: {
    identifier: 'my-invalid-name',
  },

  name: '⚙️ API',
};

export const IncompatibleIdentifier: StoryObj<typeof GqlCompatibilityWarning> =
  {
    render: () => <GqlCompatibilityWarning identifier="my-invalid-name" />,

    name: '🧰 Basic - Incompatible identifier',

    parameters: {
      docs: {
        source: { state: 'open' },
      },
    },
  };

export const IncompatibleIdentifierWithFix: StoryObj<
  typeof GqlCompatibilityWarning
> = {
  render: () => (
    <GqlCompatibilityWarning identifier="my-invalid-name" ifWarningCanBeFixed />
  ),

  name: '🎭 Variant - Fixable warning',

  parameters: {
    docs: {
      description: {
        story: `When \`ifWarningCanBeFixed\` is set, the tooltip explains that invalid characters will be automatically replaced.`,
      },
      source: { state: 'open' },
    },
  },
};

export const CompatibleIdentifier: StoryObj<typeof GqlCompatibilityWarning> = {
  render: () => <GqlCompatibilityWarning identifier="my_valid_name" />,

  name: '🔁 State - Compatible identifier (renders nothing)',

  parameters: {
    docs: {
      description: {
        story: `When the identifier is already GraphQL-compatible, the component renders \`null\`.`,
      },
      source: { state: 'open' },
    },
  },
};
