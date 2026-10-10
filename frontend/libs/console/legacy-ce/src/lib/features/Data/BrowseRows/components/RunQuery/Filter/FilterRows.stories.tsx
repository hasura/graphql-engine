import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { useConsoleForm } from '@hasura/shared/ui';
import { expect, userEvent, within } from 'storybook/test';
import { action } from 'storybook/actions';
import { z } from 'zod';
import { TableColumn } from '@hasura/metadata/data-source';
import { FilterRows } from './FilterRows';

export default {
  title: 'GDC Console/Browse Rows/parts/Run Query 📁/Filters 🧬',
  component: FilterRows,
} as Meta<typeof FilterRows>;

const columns: TableColumn[] = [
  {
    name: 'ID',
    dataType: 'number',
    consoleDataType: 'number',
    graphQLProperties: {
      name: 'ID',
      scalarType: 'Int',
    },
  },
  {
    name: 'FirstName',
    dataType: 'string',
    consoleDataType: 'string',
    graphQLProperties: {
      name: 'FirstName',
      scalarType: 'String',
    },
  },
  {
    name: 'UpdatedAt',
    dataType: 'string',
    consoleDataType: 'string',
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

export const Primary: StoryObj<typeof FilterRows> = {
  render: () => {
    const {
      methods: { watch },
      Form,
    } = useConsoleForm({
      schema: z.object({
        filters: z
          .array(
            z.object({
              column: z.string(),
              operator: z.string(),
              value: z.string(),
            }),
          )
          .optional(),
      }),
      options: {
        defaultValues: {
          filters: [
            { column: 'FirstName', operator: '_eq', value: 'John Doe' },
          ],
        },
      },
    });

    const formValues = watch('filters');

    return (
      <Form onSubmit={action('onSubmit')}>
        <>
          <FilterRows
            columns={columns}
            operators={operators}
            name="filters"
            onRemove={action('onRemove')}
          />

          <div className="py-4" data-testid="output">
            Output: {JSON.stringify(formValues)}
          </div>
        </>
      </Form>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Component should load
    await expect(
      canvas.queryByTestId('filters-filter-rows'),
    ).toBeInTheDocument();

    // The first row should be pre-populated because the default value is provided in the story
    await expect(canvas.getByTestId('filters.0.column')).toHaveValue(
      'FirstName',
    );
    await expect(canvas.getByTestId('filters.0.operator')).toHaveValue('_eq');
    await expect(canvas.getByTestId('filters.0.value')).toHaveValue('John Doe');

    // I should be able to add a new filter
    canvas.getByTestId('filters.add').click();

    // I should be able to see the new filter - in it's empty state
    await expect(canvas.getByTestId('filters.1.column')).toHaveDisplayValue(
      'Select a column',
    );
    await expect(canvas.getByTestId('filters.1.operator')).toHaveDisplayValue(
      'Select an operator',
    );
    await expect(canvas.getByTestId('filters.1.value')).toHaveDisplayValue('');

    // I should be able to fill up the values
    await userEvent.selectOptions(canvas.getByTestId('filters.1.column'), 'ID');
    await userEvent.selectOptions(
      canvas.getByTestId('filters.1.operator'),
      '_neq',
    );
    await userEvent.type(canvas.getByTestId('filters.1.value'), '123');

    // Verify if the real-time output is correct
    await expect(canvas.getByTestId('output')).toHaveTextContent(
      `Output: [{"column":"FirstName","operator":"_eq","value":"John Doe"},{"column":"ID","operator":"_neq","value":"123"}]`,
    );

    // Delete the last filter
    canvas.getByTestId('filters.1.remove').click();

    // Verify if the real-time output is correct yet again
    await expect(canvas.getByTestId('output')).toHaveTextContent(
      `Output: [{"column":"FirstName","operator":"_eq","value":"John Doe"}]`,
    );
  },
};
