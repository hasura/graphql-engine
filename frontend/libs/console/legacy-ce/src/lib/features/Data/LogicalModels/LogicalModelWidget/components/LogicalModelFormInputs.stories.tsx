import { expect } from '@storybook/jest';
import { Meta, StoryFn, StoryObj } from '@storybook/react';
import { waitFor, within } from '@storybook/testing-library';
import { SimpleForm } from '../../../../../new-components/Form';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { addLogicalModelValidationSchema } from '../validationSchema';
import { LogicalModelFormInputs } from './LogicalModelFormInputs';
import { rest } from 'msw';
import { extractTypeAndArgs } from '../../AddNativeQuery/mocks/native-query-handlers';
import { metadata } from '../mocks/metadata';

export default {
  component: LogicalModelFormInputs,
  decorators: [
    ReactQueryDecorator(),
    ReduxDecorator({
      tables: {
        dataHeaders: {
          'x-hasura-admin-secret': 'myadminsecretkey',
        } as any,
      },
    }),
  ],
  parameters: {
    msw: [
      rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
        const { type } = await extractTypeAndArgs(req);

        if (type === 'export_metadata') {
          console.log('EXPORT METADATA HANDLED');
          return res(ctx.json(metadata));
        }
      }),
    ],
  },
} as Meta<typeof LogicalModelFormInputs>;

export const Basic: StoryFn<typeof LogicalModelFormInputs> = () => (
  <SimpleForm schema={addLogicalModelValidationSchema} onSubmit={() => {}}>
    <LogicalModelFormInputs
      logicalModels={[]}
      sourceOptions={[
        { value: 'chinook', label: 'chinook' },
        { value: 'mssql', label: 'mssql' },
      ]}
    />
  </SimpleForm>
);

export const WithDefaultValues: StoryObj<typeof LogicalModelFormInputs> = {
  render: () => {
    return (
      <SimpleForm
        schema={addLogicalModelValidationSchema}
        options={{
          defaultValues: {
            dataSourceName: 'chinook',
            fields: [
              {
                name: 'id',
                type: 'int',
                typeClass: 'scalar',
              },
              {
                name: 'first_name',
                type: 'text',
                typeClass: 'scalar',
              },
            ],
            name: 'foobar',
          },
        }}
        onSubmit={() => {}}
      >
        <LogicalModelFormInputs
          logicalModels={[]}
          sourceOptions={[
            { value: 'chinook', label: 'chinook' },
            { value: 'mssql', label: 'mssql' },
          ]}
        />
      </SimpleForm>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(await canvas.findByTestId('name')).toHaveValue('foobar');
    await expect(await canvas.findByTestId('fields[0].name')).toHaveValue('id');

    // this first waitFor waits for the element to show the correct value which means the loading state has finished.
    await waitFor(async () => {
      expect(await canvas.findByTestId('fields-input-type-0')).toHaveValue(
        'scalar:int'
      );
    });

    await expect(await canvas.findByTestId('fields[1].name')).toHaveValue(
      'first_name'
    );
    await expect(await canvas.findByTestId('fields-input-type-1')).toHaveValue(
      'scalar:text'
    );
  },
};
