import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { LogicalModelFormInputs } from './LogicalModelFormInputs';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { SimpleForm } from '../../../../../new-components/Form';
import { addLogicalModelValidationSchema } from '../validationSchema';

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
} as ComponentMeta<typeof LogicalModelFormInputs>;

export const Basic: ComponentStory<typeof LogicalModelFormInputs> = () => (
  <SimpleForm schema={addLogicalModelValidationSchema} onSubmit={() => {}}>
    <LogicalModelFormInputs sourceOptions={[]} typeOptions={[]} />
  </SimpleForm>
);

export const WithDefaultValues: ComponentStory<
  typeof LogicalModelFormInputs
> = () => {
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
            },
            {
              name: 'first_name',
              type: 'text',
            },
          ],
          name: 'foobar',
        },
      }}
      onSubmit={() => {}}
    >
      <LogicalModelFormInputs
        sourceOptions={[{ value: 'chinook', label: 'chinook' }]}
        typeOptions={['text', 'int']}
      />
    </SimpleForm>
  );
};

WithDefaultValues.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await expect(await canvas.findByTestId('name')).toHaveValue('foobar');
  await expect(await canvas.findByTestId('fields[0].name')).toHaveValue('id');
  await expect(await canvas.findByTestId('fields[0].type')).toHaveValue('int');
  await expect(await canvas.findByTestId('fields[1].name')).toHaveValue(
    'first_name'
  );
  await expect(await canvas.findByTestId('fields[1].type')).toHaveValue('text');
};
