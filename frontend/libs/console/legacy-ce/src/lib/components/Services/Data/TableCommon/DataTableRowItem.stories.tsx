import React from 'react';
import { expect } from '@storybook/jest';
import { StoryObj, Meta } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import { DataTableRowItem } from './DataTableRowItem';

const values = [
  {
    column: {
      comment: null,
      data_type: 'integer',
      table_name: 'fk_test_table',
      column_name: 'id',
      is_nullable: 'NO',
      table_schema: 'public',
      column_default: "nextval('fk_test_table_id_seq'::regclass)",
      data_type_name: 'int4',
      ordinal_position: 1,
    },
    enumOptions: [],
    index: '0',
  },
  {
    column: {
      comment: null,
      data_type: 'text',
      table_name: 'fk_test_table',
      column_name: 'name',
      is_nullable: 'NO',
      table_schema: 'public',
      column_default: null,
      data_type_name: 'text',
      ordinal_position: 23,
    },
    enumOptions: [],
    index: '1',
  },
  {
    column: {
      comment: null,
      data_type: 'integer',
      table_name: 'fk_test_table',
      column_name: 'counter',
      is_nullable: 'YES',
      table_schema: 'public',
      column_default: null,
      data_type_name: 'int4',
      ordinal_position: 3,
    },
    enumOptions: [],
    index: '1',
  },
];

export default {
  title: 'Data/Services TableCommon/DataTableRowItem',
  component: DataTableRowItem,
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

export const Primary: StoryObj = {
  render: () => (
    <>
      {values.map((value, i) => {
        return (
          <DataTableRowItem
            column={value.column}
            enumOptions={value.enumOptions}
            index={String(i)}
            onColumnUpdate={() => {}}
            values={{}}
            setNullCheckedValues={() => null}
            setDefaultValueColumns={() => null}
          />
        );
      })}
    </>
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Type in text input field
    userEvent.type(await canvas.findByPlaceholderText('text'), 'John Dough');

    // Expand field
    userEvent.click(await canvas.findByText('Expand (Ctrl + Space)'));

    // Check that expanded null checkbox is not defaulting to checked
    expect(await canvas.queryByTestId('null-value-radio-1')).not.toBeChecked();

    // Collapse field
    userEvent.click(await canvas.findByText('Collapse (Ctrl + Space)'));

    // Check that collapsed null checkbox is not defaulting to checked
    expect(await canvas.queryByTestId('null-value-radio-1')).not.toBeChecked();
  },
};
