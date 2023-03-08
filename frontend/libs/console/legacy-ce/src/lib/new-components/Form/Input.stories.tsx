import { ComponentMeta, ComponentStory } from '@storybook/react';
import { FiSearch } from 'react-icons/fi';
import { Button } from '../Button';

import { Input } from './';

export default {
  title: 'components/Forms 📁/Input 🧬',
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
} as ComponentMeta<typeof Input>;

export const ApiPlayground: ComponentStory<typeof Input> = args => {
  return <Input {...args} />;
};
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  name: 'InputName',
  label: 'Play with me!',
  placeholder: 'Play with me!',
};

export const Basic: ComponentStory<typeof Input> = () => {
  return <Input name="InputName" label="The Input label" />;
};
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantClearButton: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      clearButton
    />
  );
};
VariantClearButton.storyName = '🎭 Variant - Clear button';
VariantClearButton.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantEmailType: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      type="email"
    />
  );
};
VariantEmailType.storyName = '🎭 Variant - Type email';
VariantEmailType.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantPasswordType: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      type="password"
    />
  );
};
VariantPasswordType.storyName = '🎭 Variant - Type password';
VariantPasswordType.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantFileType: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      type="file"
    />
  );
};
VariantFileType.storyName = '🎭 Variant - Type file';
VariantFileType.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantIconStart: ComponentStory<typeof Input> = () => {
  return <Input name="InputName" label="The Input label" icon={<FiSearch />} />;
};
VariantIconStart.storyName = '🎭 Variant - Icon start';
VariantIconStart.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantIconEnd: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      icon={<FiSearch />}
      iconPosition="end"
    />
  );
};
VariantIconEnd.storyName = '🎭 Variant - Icon end';
VariantIconEnd.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantPrependLabel: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      prependLabel="Prepend label"
    />
  );
};
VariantPrependLabel.storyName = '🎭 Variant - Prepend label';
VariantPrependLabel.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantAppendLabel: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      appendLabel="Append label"
    />
  );
};
VariantAppendLabel.storyName = '🎭 Variant - Append label';
VariantAppendLabel.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithRightButton: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      rightButton={<Button icon={<FiSearch />} />}
    />
  );
};
VariantWithRightButton.storyName = '🎭 Variant - Right button';
VariantWithRightButton.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateWithDefaultValue: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
    />
  );
};
StateWithDefaultValue.storyName = '🔁 State - With default value';
StateWithDefaultValue.parameters = {
  docs: {
    description: {
      story: `Use \`<SimpleForm>\` options to set default value.`,
    },
    source: { state: 'open' },
  },
};

export const StateLoading: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      loading
    />
  );
};
StateLoading.storyName = '🔁 State - Loading';
StateLoading.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateDisabled: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="The Input label"
      placeholder="The Input placeholder"
      disabled
    />
  );
};
StateDisabled.storyName = '🔁 State - Disabled';
StateDisabled.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const TestingScalability: ComponentStory<typeof Input> = () => {
  return (
    <Input
      name="InputName"
      label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      placeholder="--Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.--"
      description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    />
  );
};
TestingScalability.storyName = '🧪 Testing - Scalability';
TestingScalability.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
