import { StoryObj, Meta } from '@storybook/react-webpack5';
import { FiSearch } from 'react-icons/fi';
import { Input } from './index';
import { IconButton } from '@radix-ui/themes';

export default {
  title: 'components/Forms 📁/base/Input 🧬',
  component: Input,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<input>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input)),
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<SimpleForm>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof Input>;

export const ApiPlayground: StoryObj<typeof Input> = {
  render: (args) => {
    return <Input {...args} />;
  },

  name: '⚙️ API',

  args: {
    name: 'InputName',
    placeholder: 'Play with me!',
  },
};

export const Basic: StoryObj<typeof Input> = {
  render: () => {
    return <Input name="InputName" />;
  },

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantClearButton: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input name="InputName" placeholder="The Input placeholder" clearable />
    );
  },

  name: '🎭 Variant - Clear button',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantEmailType: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input
        name="InputName"
        placeholder="The Input placeholder"
        type="email"
      />
    );
  },

  name: '🎭 Variant - Type email',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantPasswordType: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input
        name="InputName"
        placeholder="The Input placeholder"
        type="password"
      />
    );
  },

  name: '🎭 Variant - Type password',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantIconStart: StoryObj<typeof Input> = {
  render: () => {
    return <Input name="InputName" icon={FiSearch} />;
  },

  name: '🎭 Variant - Icon start',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantIconEnd: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input
        name="InputName"
        placeholder="The Input placeholder"
        icon={FiSearch}
        iconPosition="end"
      />
    );
  },

  name: '🎭 Variant - Icon end',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantPrependLabel: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input
        name="InputName"
        placeholder="The Input placeholder"
        prependLabel="Prepend label"
      />
    );
  },

  name: '🎭 Variant - Prepend label',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantAppendLabel: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input
        name="InputName"
        placeholder="The Input placeholder"
        appendLabel="Append label"
      />
    );
  },

  name: '🎭 Variant - Append label',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantWithRightButton: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input
        name="InputName"
        placeholder="The Input placeholder"
        rightButton={
          <IconButton>
            <FiSearch />
          </IconButton>
        }
      />
    );
  },

  name: '🎭 Variant - Right button',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateWithDefaultValue: StoryObj<typeof Input> = {
  render: () => {
    return <Input name="InputName" placeholder="The Input placeholder" />;
  },

  name: '🔁 State - With default value',

  parameters: {
    docs: {
      description: {
        story: `Use \`<SimpleForm>\` options to set default value.`,
      },
      source: { state: 'open' },
    },
  },
};

export const StateLoading: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input name="InputName" placeholder="The Input placeholder" disabled />
    );
  },

  name: '🔁 State - Loading',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateDisabled: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input name="InputName" placeholder="The Input placeholder" disabled />
    );
  },

  name: '🔁 State - Disabled',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const TestingScalability: StoryObj<typeof Input> = {
  render: () => {
    return (
      <Input
        name="InputName"
        placeholder="--Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.--"
      />
    );
  },

  name: '🧪 Testing - Scalability',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
