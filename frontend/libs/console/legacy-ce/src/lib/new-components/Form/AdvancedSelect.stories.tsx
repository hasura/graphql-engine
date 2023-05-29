import React from 'react';
import { FaPlug, FaTable } from 'react-icons/fa';
import { action } from '@storybook/addon-actions';
import { StoryObj, Meta } from '@storybook/react';
import { expect } from '@storybook/jest';
import { userEvent } from '@storybook/testing-library';
import { within } from '@storybook/testing-library';
import { AdvancedSelect } from '.';

export default {
  title: 'components/Forms 📁/Advanced Select 🧬',
  component: AdvancedSelect,
  parameters: {
    docs: {
      source: { type: 'code' },
    },
  },
} as Meta<typeof AdvancedSelect>;

export const Basic: StoryObj<typeof AdvancedSelect> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    return (
      <AdvancedSelect
        name="selectNames"
        label="The select label"
        options={options}
        onChange={action('onChange')}
        onBlur={action('onBlur')}
      />
    );
  },

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Open select
    await userEvent.click(await canvas.findByText('Select...'));

    // Select first element
    await userEvent.click(await canvas.findByText('Value 0'));

    // Verify placeholder is gone and it's replaced by selected value
    expect(canvas.queryByText('Select...')).not.toBeInTheDocument();
    expect(canvas.getByText('Value 0')).toBeInTheDocument();

    // Open select
    await userEvent.click(await canvas.findByText('Value 0'));

    // Select non disabled element
    await userEvent.click(await canvas.findByText('Value 2'));

    // New element is selected
    expect(await canvas.getByText('Value 2')).toBeInTheDocument();

    // Open select
    await userEvent.click(await canvas.findByText('Value 2'));

    // Click on disabled element
    await userEvent.click(await canvas.findByText('Value 1'));

    // Click on canvas to close select
    await userEvent.click(canvasElement);

    // Verify disabled element is not selected
    expect(await canvas.queryByText('Value 1')).not.toBeInTheDocument();

    // Verify 'Value 2' is still selected
    expect(await canvas.getByText('Value 2')).toBeInTheDocument();
  },
};

export const Multi: StoryObj<typeof AdvancedSelect> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    return (
      <AdvancedSelect
        name="selectNames"
        label="The select label"
        options={options}
        isMulti
        onChange={action('onChange')}
        onBlur={action('onBlur')}
      />
    );
  },

  name: '🎭 Variant - Multi selection',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Open select
    await userEvent.click(await canvas.findByText('Select...'));

    // Select first element
    await userEvent.click(await canvas.findByText('Value 0'));

    // Verify placeholder is gone and it's replaced by selected value
    expect(canvas.queryByText('Select...')).not.toBeInTheDocument();
    expect(canvas.getByText('Value 0')).toBeInTheDocument();

    // Open select
    await userEvent.click(await canvas.findByText('Value 0'));

    // Select non disabled element
    await userEvent.click(await canvas.findByText('Value 2'));

    // Verify both elements are selected
    expect(canvas.getByText('Value 0')).toBeInTheDocument();
    expect(canvas.getByText('Value 2')).toBeInTheDocument();

    // Open select
    await userEvent.click(await canvas.findByText('Value 0'));

    // Click on disabled element
    await userEvent.click(await canvas.findByText('Value 1'));

    // Click on canvas to close select
    await userEvent.click(canvasElement);

    // Verify disabled element is not selected
    expect(canvas.queryByText('Value 1')).not.toBeInTheDocument();
  },
};

export const TableSelection: StoryObj<typeof AdvancedSelect> = {
  render: () => {
    const options = [
      {
        value: 'a_database.public.users',
        label: (
          <>
            <FaTable fill="fill-slate-900" />
            <span className="text-slate-500 ml-1.5">a_database / public /</span>
            <span className="text-slate-900 ml-1">users</span>
          </>
        ),
      },
      {
        value: 'user_schema.users',
        label: (
          <>
            <FaPlug fill="fill-slate-900" />
            <span className="text-slate-900 ml-1.5">user_schema /</span>
            <span className="text-muted ml-1">users</span>
          </>
        ),
      },
    ];

    return (
      <AdvancedSelect
        name="selectTable"
        label="From source"
        options={options}
        placeholder="Select a reference..."
        onChange={action('onChange')}
        onBlur={action('onBlur')}
      />
    );
  },

  name: '📄 Use Case - Table Selection',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
