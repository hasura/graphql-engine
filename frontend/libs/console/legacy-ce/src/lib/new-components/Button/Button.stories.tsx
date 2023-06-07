import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { Button } from '.';
import { FiSearch } from 'react-icons/fi';

export default {
  title: 'components/Button ⚛️',
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<button>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button)).
It supports a loading state. The button inner content is added in a dedicated **slot**.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with \`padding: 1rem;\`)`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    Story => (
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
  component: Button,
} as Meta<typeof Button>;

export const ApiPlayground: StoryObj<typeof Button> = {
  render: args => (
    <Button {...args} onClick={action('onClick')}>
      <span>The button children</span>
    </Button>
  ),

  name: '⚙️ API',
};

export const Basic: StoryObj<typeof Button> = {
  render: () => (
    <Button onClick={action('onClick')}>
      <span>The button children</span>
    </Button>
  ),

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantMode: StoryObj<typeof Button> = {
  render: () => (
    <>
      <Button mode="default" onClick={action('onClick')}>
        <span>Default</span>
      </Button>
      <Button mode="primary" onClick={action('onClick')}>
        <span>Primary</span>
      </Button>
      <Button mode="destructive" onClick={action('onClick')}>
        <span>Destructive</span>
      </Button>
      <Button mode="success" onClick={action('onClick')}>
        <span>Success</span>
      </Button>
    </>
  ),

  name: '🎭 Variant - Mode',

  parameters: {
    docs: {
      description: {
        story: `#### 🚦 Usage
  - The default button should be used in the vast majority of circumstances
  - The primary button style should only be used once per page for create / persistent actions, we also shouldn't have
    any other sizes for the primary button
  - Use destructive variant for destructive actions only`,
      },
      source: { state: 'open' },
    },
  },
};

export const VariantSize: StoryObj<typeof Button> = {
  render: () => (
    <>
      <Button size="sm" onClick={action('onClick')}>
        <span>The sm button</span>
      </Button>
      <Button size="md" onClick={action('onClick')}>
        <span>The md button</span>
      </Button>
      <Button size="lg" onClick={action('onClick')}>
        <span>The lg button</span>
      </Button>
    </>
  ),

  name: '🎭 Variant - Size',

  parameters: {
    docs: {
      description: {
        story: `#### 🚦 Usage
  - Small is typically used for inline-forms while the large button is used with in coordination.`,
      },
      source: { state: 'open' },
    },
  },
};

export const VariantIcon: StoryObj<typeof Button> = {
  render: () => (
    <>
      <Button
        icon={<FiSearch />}
        iconPosition="start"
        onClick={action('onClick')}
      >
        <span>Icon start</span>
      </Button>
      <Button
        icon={<FiSearch />}
        iconPosition="end"
        onClick={action('onClick')}
      >
        <span>Icon end</span>
      </Button>
      <Button icon={<FiSearch />} onClick={action('onClick')} />
    </>
  ),

  name: '🎭 Variant - Icon',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateLoading: StoryObj<typeof Button> = {
  render: () => (
    <div className="flex flex-col gap-4">
      <div className="flex gap-2">
        <Button
          isLoading
          loadingText="Loading..."
          mode="default"
          onClick={action('onClick')}
        >
          <span>Loading</span>
        </Button>
        <Button
          isLoading
          loadingText="Loading..."
          mode="primary"
          onClick={action('onClick')}
        >
          <span>Loading</span>
        </Button>
        <Button
          isLoading
          loadingText="Loading..."
          mode="destructive"
          onClick={action('onClick')}
        >
          <span>Loading</span>
        </Button>
        <Button
          isLoading
          loadingText="Loading..."
          mode="success"
          onClick={action('onClick')}
        >
          <span>Loading</span>
        </Button>
      </div>
      <div className="flex gap-2">
        <Button
          isLoading
          loadingText="Loading..."
          size="sm"
          onClick={action('onClick')}
        >
          <span>Loading</span>
        </Button>
        <Button
          isLoading
          loadingText="Loading..."
          size="md"
          onClick={action('onClick')}
        >
          <span>Loading</span>
        </Button>
        <Button
          isLoading
          loadingText="Loading..."
          size="lg"
          onClick={action('onClick')}
        >
          <span>Loading</span>
        </Button>
      </div>
      <div className="flex gap-2">
        <Button
          isLoading
          loadingText="Loading..."
          icon={<FiSearch />}
          iconPosition="start"
          onClick={action('onClick')}
        >
          <span>Icon start</span>
        </Button>
        <Button
          isLoading
          loadingText="Loading..."
          icon={<FiSearch />}
          iconPosition="end"
          onClick={action('onClick')}
        >
          <span>Icon end</span>
        </Button>
        <Button
          isLoading
          loadingText="Loading..."
          icon={<FiSearch />}
          onClick={action('onClick')}
        />
      </div>
    </div>
  ),

  name: '🔁 State - Loading',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateDisabled: StoryObj<typeof Button> = {
  render: () => (
    <div className="flex flex-col gap-4">
      <div className="flex gap-2">
        <Button disabled mode="default" onClick={action('onClick')}>
          <span>Disabled</span>
        </Button>
        <Button disabled mode="primary" onClick={action('onClick')}>
          <span>Disabled</span>
        </Button>
        <Button disabled mode="destructive" onClick={action('onClick')}>
          <span>Disabled</span>
        </Button>
        <Button disabled mode="success" onClick={action('onClick')}>
          <span>Disabled</span>
        </Button>
      </div>
      <div className="flex gap-2">
        <Button disabled size="sm" onClick={action('onClick')}>
          <span>Disabled</span>
        </Button>
        <Button disabled size="md" onClick={action('onClick')}>
          <span>Disabled</span>
        </Button>
        <Button disabled size="lg" onClick={action('onClick')}>
          <span>Disabled</span>
        </Button>
      </div>
      <div className="flex gap-2">
        <Button
          disabled
          icon={<FiSearch />}
          iconPosition="start"
          onClick={action('onClick')}
        >
          <span>Disabled</span>
        </Button>
        <Button
          disabled
          icon={<FiSearch />}
          iconPosition="end"
          onClick={action('onClick')}
        >
          <span>Disabled</span>
        </Button>
        <Button disabled icon={<FiSearch />} onClick={action('onClick')} />
      </div>
    </div>
  ),

  name: '🔁 State - Disabled',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const TestingScalability: StoryObj<typeof Button> = {
  render: () => (
    <Button onClick={action('onClick')}>
      <div className="max-w-full text-ellipsis overflow-hidden">
        Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
        tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
        veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
        commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
        velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
        occaecat cupidatat non proident, sunt in culpa qui officia deserunt
        mollit anim id est laborum.
      </div>
    </Button>
  ),

  name: '🧪 Testing - Scalability',

  parameters: {
    docs: {
      description: {
        story: `⚠️ Please add some defensive checks in the component children to prevent them to overflow.`,
      },
      source: { state: 'open' },
    },
  },
};
