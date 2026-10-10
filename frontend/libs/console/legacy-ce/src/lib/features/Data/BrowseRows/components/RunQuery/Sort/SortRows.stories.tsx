import { StoryObj, Meta } from '@storybook/react-webpack5';
import { z } from 'zod';
import { FormDecorator } from '@hasura/shared/testing';
import { expect, userEvent, within } from 'storybook/test';
import { action } from 'storybook/actions';
import { TableColumn } from '@hasura/metadata/data-source';
import { useConsoleForm } from '@hasura/shared/ui';
import { SortRows } from './SortRows';

export default {
  title: 'GDC Console/Browse Rows/parts/Run Query 📁/Sort 🧬',
  component: SortRows,
  decorators: [FormDecorator()],
} as Meta<typeof SortRows>;

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

export const Primary: StoryObj<typeof SortRows> = {
  render: () => {
    const {
      methods: { watch },
      Form,
    } = useConsoleForm({
      schema: z.object({
        sorts: z
          .array(
            z.object({
              column: z.string(),
              type: z.literal('asc').or(z.literal('desc')),
            }),
          )
          .optional(),
      }),
      options: {
        defaultValues: {
          sorts: [{ column: 'FirstName', type: 'asc' }],
        },
      },
    });

    const formValues = watch('sorts');

    return (
      <Form onSubmit={action('onSubmit')}>
        <div className="w-1/2">
          <SortRows
            columns={columns}
            name="sorts"
            onRemove={action('onRemove')}
          />

          <div className="py-4" data-testid="output">
            Output: {JSON.stringify(formValues)}
          </div>
        </div>
      </Form>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Component should load
    await expect(canvas.queryByTestId('sorts-sort-rows')).toBeInTheDocument();

    // The first row should be pre-populated because the default value is provided in the story
    await expect(canvas.getByTestId('sorts.0.column')).toHaveValue('FirstName');
    await expect(canvas.getByTestId('sorts.0.type')).toHaveValue('asc');

    // I should be able to add a new filter
    canvas.getByTestId('sorts.add').click();

    // I should be able to see the new filter - in it's empty state
    await expect(canvas.getByTestId('sorts.1.column')).toHaveDisplayValue(
      'Select a column',
    );
    await expect(canvas.getByTestId('sorts.1.type')).toHaveDisplayValue(
      'Order by',
    );

    // I should be able to fill up the values
    await userEvent.selectOptions(canvas.getByTestId('sorts.1.column'), 'ID');
    await userEvent.selectOptions(canvas.getByTestId('sorts.1.type'), 'asc');

    // Verify if the real-time output is correct
    await expect(canvas.getByTestId('output')).toHaveTextContent(
      `Output: [{"column":"FirstName","type":"asc"},{"column":"ID","type":"asc"}]`,
    );

    // Delete the last filter
    canvas.getByTestId('sorts.1.remove').click();

    // Verify if the real-time output is correct yet again
    await expect(canvas.getByTestId('output')).toHaveTextContent(
      `Output: [{"column":"FirstName","type":"asc"}]`,
    );
  },
};
