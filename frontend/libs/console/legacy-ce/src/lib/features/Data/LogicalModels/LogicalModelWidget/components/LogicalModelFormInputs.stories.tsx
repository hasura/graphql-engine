import { expect, waitFor, within } from 'storybook/test';
import { Meta, StoryFn, StoryObj } from '@storybook/react-webpack5';
import { SimpleForm } from '@hasura/shared/ui';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { addLogicalModelValidationSchema } from '../validationSchema';
import { LogicalModelFormInputs } from './LogicalModelFormInputs';
import { http, HttpResponse } from 'msw';
import { extractTypeAndArgs } from '../../AddNativeQuery/mocks/native-query-handlers';
import { metadata } from '../mocks/metadata';

export default {
  component: LogicalModelFormInputs,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: [
      http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
        const { type } = await extractTypeAndArgs(request);

        if (type === 'export_metadata') {
          console.log('EXPORT METADATA HANDLED');
          return HttpResponse.json(metadata);
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
      await expect(
        await canvas.findByTestId('fields-input-type-0'),
      ).toHaveValue('scalar:int');
    });

    await expect(await canvas.findByTestId('fields[1].name')).toHaveValue(
      'first_name',
    );
    await expect(await canvas.findByTestId('fields-input-type-1')).toHaveValue(
      'scalar:text',
    );
  },
};
