import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { UpdatedForm } from '@/new-components/Form';
import { z } from 'zod';
import { FormDecorator } from '@/storybook/decorators/react-hook-form';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { SortRows } from './SortRows';

export default {
  title: 'Browse Rows/Run Query/Sort',
  component: SortRows,
  decorators: [FormDecorator()],
} as ComponentMeta<typeof SortRows>;

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

export const Primary: ComponentStory<typeof SortRows> = () => {
  return (
    <UpdatedForm
      schema={z.object({
        sorts: z
          .array(
            z.object({
              column: z.string(),
              type: z.literal('asc').or(z.literal('desc')),
            })
          )
          .optional(),
      })}
      options={{
        defaultValues: {
          sorts: [{ column: 'FirstName', type: 'asc' }],
        },
      }}
      onSubmit={data => {
        console.log(data);
      }}
    >
      {({ watch }) => {
        const formValues = watch('sorts');
        return (
          <div className="w-1/2">
            <SortRows columns={columns} name="sorts" />

            <div className="py-4" data-testid="output">
              Output: {JSON.stringify(formValues)}
            </div>
          </div>
        );
      }}
    </UpdatedForm>
  );
};

Primary.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // Component should load
  expect(canvas.queryByTestId('sorts-sort-rows')).toBeInTheDocument();

  // The first row should be pre-populated because the default value is provided in the story
  expect(canvas.getByTestId('sorts.0.column')).toHaveValue('FirstName');
  expect(canvas.getByTestId('sorts.0.type')).toHaveValue('asc');

  // I should be able to add a new filter
  canvas.getByTestId('sorts.add').click();

  // I should be able to see the new filter - in it's empty state
  expect(canvas.getByTestId('sorts.1.column')).toHaveDisplayValue(
    'Select a column'
  );
  expect(canvas.getByTestId('sorts.1.type')).toHaveDisplayValue('Order by');

  // I should be able to fill up the values
  userEvent.selectOptions(canvas.getByTestId('sorts.1.column'), 'ID');
  userEvent.selectOptions(canvas.getByTestId('sorts.1.type'), 'asc');

  // Verify if the real-time output is correct
  expect(canvas.getByTestId('output')).toHaveTextContent(
    `Output: [{"column":"FirstName","type":"asc"},{"column":"ID","type":"asc"}]`
  );

  // Delete the last filter
  canvas.getByTestId('sorts.1.remove').click();

  // Verify if the real-time output is correct yet again
  expect(canvas.getByTestId('output')).toHaveTextContent(
    `Output: [{"column":"FirstName","type":"asc"}]`
  );
};
