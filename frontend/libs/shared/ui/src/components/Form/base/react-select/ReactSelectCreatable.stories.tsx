import type { Meta, StoryObj } from '@storybook/react-webpack5';
import { action } from 'storybook/actions';

import { ReactSelectCreatable } from './ReactSelectCreatable';

export default {
  title: 'components/Forms 📁/base/ReactSelectCreatable ⚛️',
  component: ReactSelectCreatable,
  parameters: {
    docs: {
      description: {
        component: `An async, creatable [react-select](https://react-select.com/creatable) — options are (optionally) loaded asynchronously via \`loadOptions\`, and the user can type a value that isn't in the list to create a new option.`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof ReactSelectCreatable>;

const FLAVOURS = [
  { value: 'chocolate', label: 'Chocolate' },
  { value: 'strawberry', label: 'Strawberry' },
  { value: 'vanilla', label: 'Vanilla' },
];

const loadOptions = (inputValue: string) =>
  Promise.resolve(
    FLAVOURS.filter((option) =>
      option.label.toLowerCase().includes(inputValue.toLowerCase()),
    ),
  );

export const Basic: StoryObj<typeof ReactSelectCreatable> = {
  name: '🧰 Basic',
  args: {
    options: FLAVOURS,
    loadOptions,
    onChange: action('onChange'),
    onCreateOption: action('onCreateOption'),
    classNamePrefix: 'react-select',
    placeholder: 'Select or create an option...',
  },
};

export const StateWithDefaultValue: StoryObj<typeof ReactSelectCreatable> = {
  ...Basic,
  name: '🔁 State - With default value',
  args: {
    ...Basic.args,
    defaultValue: { value: 'vanilla', label: 'Vanilla' },
  },
};

export const StateDisabled: StoryObj<typeof ReactSelectCreatable> = {
  ...Basic,
  name: '🔁 State - Disabled',
  args: {
    ...Basic.args,
    isDisabled: true,
  },
};

export const StateInvalid: StoryObj<typeof ReactSelectCreatable> = {
  ...Basic,
  name: '🔁 State - Invalid',
  args: {
    ...Basic.args,
    isInvalid: true,
  },
};

export const VariantMulti: StoryObj<typeof ReactSelectCreatable> = {
  ...Basic,
  name: '🎭 Variant - Multi',
  args: {
    ...Basic.args,
    isMulti: true,
    closeMenuOnSelect: false,
  },
};
