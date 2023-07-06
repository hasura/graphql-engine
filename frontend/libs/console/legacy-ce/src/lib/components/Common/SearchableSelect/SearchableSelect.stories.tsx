import React from 'react';
import { Meta, StoryObj } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import SearchableSelect from './SearchableSelect';

type Option = { label: string; value: string };

export default {
  component: SearchableSelect,
} satisfies Meta<typeof SearchableSelect>;

export const Basic: StoryObj<typeof SearchableSelect> = {
  name: '🧰 Basic',
  render: args => {
    const [value, setValue] = React.useState<Option | string | undefined>(
      args.value
    );
    const handleOnChange = (value: any) => {
      setValue(value);
      args.onChange(value);
    };
    return (
      <>
        <SearchableSelect {...args} value={value} onChange={handleOnChange} />
        <span>{JSON.stringify(value, null, '\t')}</span>
      </>
    );
  },
  args: {
    onChange: action('onChange'),
    createNewOption: action('createNewOption'),
    onSearchValueChange: action('onSearchValueChange'),
    filterOption: 'prefix',
    placeholder: 'The select placeholder',
    options: [
      {
        label: 'Group 1',
        options: [
          { label: 'Option 1', value: 'option1' },
          { label: 'Option 2', value: 'option2' },
          { label: 'Option 3', value: 'option3' },
        ],
      },
      {
        label: 'Group 2',
        options: [
          { label: 'Option 4', value: 'option4' },
          { label: 'Option 5', value: 'option5' },
          { label: 'Option 6', value: 'option6' },
        ],
      },
    ],
    value: { label: 'Option 2', value: 'option2' },
    isCreatable: true,
  },
};
