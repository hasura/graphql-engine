import { expect } from '@storybook/jest';
import React from 'react';
import type { Meta, StoryObj } from '@storybook/react';
import { ReactSelect } from './ReactSelect';
import { action } from '@storybook/addon-actions';
import { FaCandyCane, FaPlug, FaTable } from 'react-icons/fa';
import { userEvent, within } from '@storybook/testing-library';

export default {
  title: 'components/Forms 📁/Select ⚛️',
  component: ReactSelect,
} satisfies Meta<typeof ReactSelect>;

const FLAVOURS = [
  { value: 'chocolate', label: 'Chocolate' },
  { value: 'strawberry', label: 'Strawberry', isDisabled: true },
  { value: 'vanilla', label: 'Vanilla' },
];

const DATABASES = [
  {
    value: 'a_database.public.users',
    icon: FaTable,
    label: 'a_database / public / users',
  },
  {
    value: 'user_schema.users',
    icon: FaPlug,
    label: 'user_schema / users',
  },
];

export const Basic: StoryObj<typeof ReactSelect> = {
  name: '🧰 Basic',
  args: {
    options: [...FLAVOURS, ...DATABASES],
    onChange: action('onChange'),
    classNamePrefix: 'react-select',
    placeholder: 'Select an option...',
    noOptionsMessage: () => 'No matching options',
  },
};

export const StateWithDefaultValue: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🔁 State - With default value',
  args: {
    ...Basic.args,
    defaultValue: { value: 'vanilla', label: 'Vanilla' },
  },
};

export const StateWithFancyDefaultValue: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🔁 State - With fancy default value',
  args: {
    ...Basic.args,
    defaultValue: {
      value: 'user_schema.users',
      icon: FaPlug,
      label: 'user_schema / users',
    },
  },
};

export const StateDisabled: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🔁 State - Disabled',
  args: {
    ...Basic.args,
    isDisabled: true,
  },
};

export const StateInvalid: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🔁 State - Invalid',
  args: {
    ...Basic.args,
    isInvalid: true,
  },
};

export const VariantMulti: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🎭 Variant - Multi',
  args: {
    ...Basic.args,
    isMulti: true,
    closeMenuOnSelect: false,
  },
};

export const VariantMultiWithDefaultValues: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🎭 Variant - Multi with default values',
  args: {
    ...Basic.args,
    isMulti: true,
    closeMenuOnSelect: false,
    defaultValue: [
      { value: 'chocolate', label: 'Chocolate' },
      { value: 'strawberry', label: 'Strawberry' },
    ],
  },
};

export const VariantMultiWithDefaultFancyValues: StoryObj<typeof ReactSelect> =
  {
    ...Basic,
    name: '🎭 Variant - Multi with default fancy values',
    args: {
      ...Basic.args,
      isMulti: true,
      closeMenuOnSelect: false,
      defaultValue: [
        { value: 'chocolate', label: 'Chocolate' },
        { value: 'strawberry', label: 'Strawberry' },
        {
          value: 'user_schema.users',
          icon: FaPlug,
          label: 'user_schema / users',
        },
      ],
    },
  };

export const TestSingleValue: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🧪 Test - Single value',
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Open select
    await userEvent.click(await canvas.findByText('Select an option...'));

    // Select first element
    await userEvent.click(await canvas.findByText('Chocolate'), undefined, {
      skipHover: true,
    });

    // Verify placeholder is gone and it's replaced by selected value
    expect(canvas.queryByText('Select an option...')).not.toBeInTheDocument();
    await expect(canvas.getByText('Chocolate')).toBeInTheDocument();

    // Open select
    await userEvent.click(await canvas.findByText('Chocolate'));

    // Select non disabled element
    await userEvent.click(await canvas.findByText('Vanilla'), undefined, {
      skipHover: true,
    });

    // New element is selected
    await expect(await canvas.getByText('Vanilla')).toBeInTheDocument();

    // Open select
    await userEvent.click(await canvas.findByText('Vanilla'), undefined, {
      skipHover: true,
    });

    // Click on disabled element
    await userEvent.click(await canvas.findByText('Strawberry'), undefined, {
      skipHover: true,
    });

    // Click on canvas to close select
    await userEvent.click(canvasElement);

    // Verify disabled element is not selected
    expect(await canvas.queryByText('Strawberry')).not.toBeInTheDocument();

    // Verify 'Vanilla' is still selected
    await expect(await canvas.getByText('Vanilla')).toBeInTheDocument();

    // Remove selected value
    await userEvent.click(
      canvasElement?.getElementsByClassName(
        'react-select__clear-indicator'
      )[0] as HTMLElement
    );

    // Verify 'Vanilla' has been removed
    await expect(await canvas.queryByText('Vanilla')).not.toBeInTheDocument();
  },
};

export const TestMultiValue: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🧪 Test - Multi value',
  args: {
    ...Basic.args,
    isMulti: true,
    closeMenuOnSelect: false,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Open select
    await userEvent.click(await canvas.findByText('Select an option...'));

    // Select first element
    await userEvent.click(await canvas.findByText('Chocolate'), undefined, {
      skipHover: true,
    });

    // Verify placeholder is gone and it's replaced by selected value
    expect(canvas.queryByText('Select an option...')).not.toBeInTheDocument();
    await expect(await canvas.findByText('Chocolate')).toBeInTheDocument();

    // Select non disabled element
    await userEvent.click(await canvas.findByText('Vanilla'), undefined, {
      skipHover: true,
    });

    // Verify both elements are selected
    await expect(await canvas.findByText('Vanilla')).toBeInTheDocument();
    await expect(await canvas.findByText('Chocolate')).toBeInTheDocument();

    // Click on disabled element
    await userEvent.click(await canvas.findByText('Strawberry'), undefined, {
      skipHover: true,
    });

    // Click on canvas to close select
    await userEvent.click(canvasElement);

    // Verify disabled element is not selected
    expect(canvas.queryByText('Strawberry')).not.toBeInTheDocument();

    // Remove 'Chocolate' value
    await userEvent.click(
      canvasElement?.getElementsByClassName(
        'react-select__multi-value__remove'
      )[0] as HTMLElement
    );

    // Verify 'Chocolate' has been removed
    await expect(await canvas.queryByText('Chocolate')).not.toBeInTheDocument();
    await expect(await canvas.queryByText('Vanilla')).toBeInTheDocument();

    // Remove selected value
    await userEvent.click(
      canvasElement?.getElementsByClassName(
        'react-select__clear-indicator'
      )[0] as HTMLElement
    );

    // Verify 'Vanilla' has been removed
    await expect(await canvas.queryByText('Vanilla')).not.toBeInTheDocument();
  },
};

export const VariantGrouped: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🎭 Variant - Grouped',
  args: {
    ...Basic.args,
    options: [
      {
        label: 'Icecream',
        options: FLAVOURS,
      },
      {
        label: 'Databases',
        options: DATABASES,
      },
    ],
  },
};

export const Test1000Options: StoryObj<typeof ReactSelect> = {
  ...Basic,
  name: '🧪 Test - 10000 options',
  args: {
    ...Basic.args,
    options: Array.from({ length: 10000 }, (_, i) => ({
      icon: FaCandyCane,
      label: `Option ${i} label`,
      value: `option${i}-value`,
    })),
  },
};
