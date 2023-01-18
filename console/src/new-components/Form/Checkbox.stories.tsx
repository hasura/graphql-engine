import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';

import { Checkbox } from '@/new-components/Form/';

export default {
  title: 'components/Forms 📁/Checkbox ⚛️',
  component: Checkbox,
  decorators: [Story => <div className="p-4">{Story()}</div>],
  parameters: {
    docs: {
      description: {
        component: `This component is wrapping a [radix-ui checkbox component](https://www.radix-ui.com/docs/primitives/components/checkbox)<br>
Default CSS display is \`inline\`, provided without padding and margin (displayed here with \`padding: 1rem;\`)`,
      },
      source: { type: 'code' },
    },
  },
} as ComponentMeta<typeof Checkbox>;

export const ApiPlayground: ComponentStory<typeof Checkbox> = args => (
  <Checkbox {...args} />
);
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  children: <span>Checkbox</span>,
};

export const Basic: ComponentStory<typeof Checkbox> = () => (
  <>
    <Checkbox />
    <Checkbox>
      <span>The checkbox children</span>
    </Checkbox>
  </>
);
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateDisabled: ComponentStory<typeof Checkbox> = () => (
  <>
    <Checkbox disabled />
    <Checkbox disabled>
      <span>The disabled checkbox children</span>
    </Checkbox>
    <Checkbox disabled defaultChecked>
      <span>The disabled default checked checkbox children</span>
    </Checkbox>
  </>
);
StateDisabled.storyName = '🔁 State - Disabled';
StateDisabled.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateChecked: ComponentStory<typeof Checkbox> = () => (
  <>
    <Checkbox defaultChecked />
    <Checkbox defaultChecked>
      <span>The default checked checkbox children</span>
    </Checkbox>
    <Checkbox>
      <span>The checkbox children</span>
    </Checkbox>
  </>
);
StateChecked.storyName = '🔁 State - Checked';
StateChecked.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateIndeterminate: ComponentStory<typeof Checkbox> = () => (
  <>
    <Checkbox checked="indeterminate">
      <span>The indeterminate checkbox children</span>
    </Checkbox>
    <Checkbox>
      <span>The checkbox children</span>
    </Checkbox>
  </>
);
StateIndeterminate.storyName = '🔁 State - Indeterminate';
StateIndeterminate.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateInvalid: ComponentStory<typeof Checkbox> = () => (
  <>
    <Checkbox defaultChecked invalid />
    <Checkbox defaultChecked invalid>
      <span>The invalid default checked checkbox children</span>
    </Checkbox>
    <Checkbox checked="indeterminate" invalid>
      <span>The invalid indeterminate checkbox children</span>
    </Checkbox>
    <Checkbox invalid>
      <span>The invalid checkbox children</span>
    </Checkbox>
    <Checkbox defaultChecked disabled invalid>
      <span>The invalid disabled default checked checkbox children</span>
    </Checkbox>
    <Checkbox checked="indeterminate" disabled invalid>
      <span>The invalid disabled indeterminate checkbox children</span>
    </Checkbox>
    <Checkbox invalid disabled>
      <span>The invalid disabled checkbox children</span>
    </Checkbox>
  </>
);
StateInvalid.storyName = '🔁 State - Invalid';
StateInvalid.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
