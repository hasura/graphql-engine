import { StoryObj, Meta } from '@storybook/react-webpack5';
import { Checkbox } from './index';

export default {
  title: 'components/Forms 📁/base/Checkbox ⚛️',
  component: Checkbox,
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
  parameters: {
    docs: {
      description: {
        component: `This component is wrapping a [radix-ui checkbox component](https://www.radix-ui.com/docs/primitives/components/checkbox)<br>
Default CSS display is \`inline\`, provided without padding and margin (displayed here with \`padding: 1rem;\`)`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof Checkbox>;

export const ApiPlayground: StoryObj<typeof Checkbox> = {
  name: '⚙️ API',

  args: {
    children: <span>Checkbox</span>,
  },
};

export const Basic: StoryObj<typeof Checkbox> = {
  render: () => (
    <>
      <Checkbox value={false} />
      <Checkbox value={true}>
        <span>The checkbox children</span>
      </Checkbox>
    </>
  ),

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateDisabled: StoryObj<typeof Checkbox> = {
  render: () => (
    <>
      <Checkbox value={false} disabled />
      <Checkbox value={false} disabled>
        <span>The disabled checkbox children</span>
      </Checkbox>
      <Checkbox value={true} disabled defaultChecked>
        <span>The disabled default checked checkbox children</span>
      </Checkbox>
    </>
  ),

  name: '🔁 State - Disabled',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateChecked: StoryObj<typeof Checkbox> = {
  render: () => (
    <>
      <Checkbox value={true} defaultChecked />
      <Checkbox value={true} defaultChecked>
        <span>The default checked checkbox children</span>
      </Checkbox>
      <Checkbox value={false}>
        <span>The checkbox children</span>
      </Checkbox>
    </>
  ),

  name: '🔁 State - Checked',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateIndeterminate: StoryObj<typeof Checkbox> = {
  render: () => (
    <>
      <Checkbox value="indeterminate">
        <span>The indeterminate checkbox children</span>
      </Checkbox>
      <Checkbox value={false}>
        <span>The checkbox children</span>
      </Checkbox>
    </>
  ),

  name: '🔁 State - Indeterminate',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateInvalid: StoryObj<typeof Checkbox> = {
  render: () => (
    <>
      <Checkbox value={true} defaultChecked invalid />
      <Checkbox value={true} defaultChecked invalid>
        <span>The invalid default checked checkbox children</span>
      </Checkbox>
      <Checkbox value="indeterminate" invalid>
        <span>The invalid indeterminate checkbox children</span>
      </Checkbox>
      <Checkbox value={true} invalid>
        <span>The invalid checkbox children</span>
      </Checkbox>
      <Checkbox value={true} defaultChecked disabled invalid>
        <span>The invalid disabled default checked checkbox children</span>
      </Checkbox>
      <Checkbox value="indeterminate" disabled invalid>
        <span>The invalid disabled indeterminate checkbox children</span>
      </Checkbox>
      <Checkbox value={true} invalid disabled>
        <span>The invalid disabled checkbox children</span>
      </Checkbox>
    </>
  ),

  name: '🔁 State - Invalid',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
