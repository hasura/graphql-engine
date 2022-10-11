import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { UpdatedForm } from '@/new-components/Form';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { z } from 'zod';
import { FilterRows } from './FilterRows';

export default {
  title: 'Browse Rows/Run Query/Filters',
  component: FilterRows,
} as ComponentMeta<typeof FilterRows>;

const columns = [
  {
    name: 'ID',
    dataType: 'int',
    graphQLProperties: {
      name: 'ID',
      scalarType: 'Int',
    },
  },
  {
    name: 'FirstName',
    dataType: 'text',
    graphQLProperties: {
      name: 'FirstName',
      scalarType: 'String',
    },
  },
  {
    name: 'UpdatedAt',
    dataType: 'datetime',
    graphQLProperties: {
      name: 'UpdatedAtCustomName',
      scalarType: 'String',
    },
  },
];

const operators = [
  {
    name: '_eq',
    value: '_eq',
  },
  {
    name: '_neq',
    value: '_neq',
  },
  {
    name: '_gte',
    value: '_gte',
  },
  {
    name: '_lte',
    value: '_lte',
  },
];

export const Primary: ComponentStory<typeof FilterRows> = () => {
  return (
    <UpdatedForm
      schema={z.object({
        filters: z
          .array(
            z.object({
              column: z.string(),
              operator: z.string(),
              value: z.string(),
            })
          )
          .optional(),
      })}
      options={{
        defaultValues: {
          filters: [
            { column: 'FirstName', operator: '_eq', value: 'John Doe' },
          ],
        },
      }}
      onSubmit={data => {
        console.log(data);
      }}
    >
      {({ watch }) => {
        const formValues = watch('filters');
        return (
          <>
            <FilterRows
              columns={columns}
              operators={operators}
              name="filters"
            />

            <div className="py-4" data-testid="output">
              Output: {JSON.stringify(formValues)}
            </div>
          </>
        );
      }}
    </UpdatedForm>
  );
};

Primary.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Component should load
  expect(canvas.queryByTestId('filters-filter-rows')).toBeInTheDocument();

  // The first row should be pre-populated because the default value is provided in the story
  expect(canvas.getByTestId('filters.0.column')).toHaveValue('FirstName');
  expect(canvas.getByTestId('filters.0.operator')).toHaveValue('_eq');
  expect(canvas.getByTestId('filters.0.value')).toHaveValue('John Doe');

  // I should be able to add a new filter
  canvas.getByTestId('filters.add').click();

  // I should be able to see the new filter - in it's empty state
  expect(canvas.getByTestId('filters.1.column')).toHaveDisplayValue(
    'Select a column'
  );
  expect(canvas.getByTestId('filters.1.operator')).toHaveDisplayValue(
    'Select an operator'
  );
  expect(canvas.getByTestId('filters.1.value')).toHaveDisplayValue('');

  // I should be able to fill up the values
  userEvent.selectOptions(canvas.getByTestId('filters.1.column'), 'ID');
  userEvent.selectOptions(canvas.getByTestId('filters.1.operator'), '_neq');
  userEvent.type(canvas.getByTestId('filters.1.value'), '123');

  // Verify if the real-time output is correct
  expect(canvas.getByTestId('output')).toHaveTextContent(
    `Output: [{"column":"FirstName","operator":"_eq","value":"John Doe"},{"column":"ID","operator":"_neq","value":"123"}]`
  );

  // Delete the last filter
  canvas.getByTestId('filters.1.remove').click();

  // Verify if the real-time output is correct yet again
  expect(canvas.getByTestId('output')).toHaveTextContent(
    `Output: [{"column":"FirstName","operator":"_eq","value":"John Doe"}]`
  );
};
