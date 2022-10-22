import { Button } from '@/new-components/Button';
import { Form } from '@/new-components/Form';
import { userEvent, within } from '@storybook/testing-library';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta, Story } from '@storybook/react';
import React from 'react';
import { expect } from '@storybook/jest';
import { Controller, useFormContext } from 'react-hook-form';
import { z } from 'zod';
import { DatabaseSelector } from '@/features/Data';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Data/components/DatabaseSelector',
  component: DatabaseSelector,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof DatabaseSelector>;

export const Playground: Story = () => {
  return (
    <DatabaseSelector
      value={{ database: 'chinook', schema: 'public', table: 'Album' }}
      onChange={v => console.log(v)}
      name="source"
      className="border-l-4 border-l-green-600"
      labels={{
        database: 'My Database',
        schema: 'My Schema',
        dataset: 'My Dataset',
        table: 'My Table',
      }}
    />
  );
};

export const WithDisabledInputs: Story = () => {
  return (
    <DatabaseSelector
      value={{ database: 'chinook', schema: 'public', table: 'Album' }}
      onChange={v => console.log(v)}
      name="source"
      className="border-l-4 border-l-green-600"
      disabledKeys={['database', 'schema', 'table']}
    />
  );
};

export const BqWithDisabledInputs: Story = () => {
  return (
    <DatabaseSelector
      value={{ database: 'bigquery_test', dataset: 'sensei', table: 'table1' }}
      onChange={v => console.log(v)}
      name="source"
      className="border-l-4 border-l-green-600"
      disabledKeys={['database', 'schema', 'table']}
    />
  );
};

export const WithHiddenInputs: Story = () => {
  return (
    <DatabaseSelector
      value={{ database: 'bigquery_test', dataset: 'sensei', table: '' }}
      onChange={v => console.log(v)}
      name="source"
      className="border-l-4 border-l-green-600"
      hiddenKeys={['database']}
      disabledKeys={['schema']}
    />
  );
};

type Schema = z.infer<typeof schema>;

const schema = z.object({
  destination: z.object({
    database: z.string().min(1, 'Database is a required field!'),
    schema: z.string().optional(),
    dataset: z.string().optional(),
    table: z.string().min(1, 'Table is a required field!'),
  }),
});

const FormElements = () => {
  const { control } = useFormContext<Schema>();

  return (
    <>
      <Controller
        control={control}
        name="destination"
        render={({ field: { onChange, value }, formState: { errors } }) => (
          <DatabaseSelector
            value={value}
            onChange={onChange}
            name="destination"
            errors={errors}
            className="border-l-4 border-l-green-600"
          />
        )}
      />
    </>
  );
};

export const WithReactFormHookNested: Story = () => {
  const submit = (values: Schema) => {
    console.log(JSON.stringify(values));
  };

  return (
    <Form
      options={{
        defaultValues: {
          destination: {
            database: '',
            schema: '',
            table: '',
          },
        },
      }}
      onSubmit={submit}
      schema={schema}
    >
      {() => {
        return (
          <>
            <FormElements />
            <Button type="submit" data-testid="submit">
              Submit
            </Button>
          </>
        );
      }}
    </Form>
  );
};

WithReactFormHookNested.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  const sourceLabel = await canvas.findByText('Source');
  const schemaLabel = await canvas.findByText('Schema');
  const tableLabel = await canvas.findByText('Table');

  // expect labels
  expect(sourceLabel).toBeInTheDocument();
  expect(schemaLabel).toBeInTheDocument();
  expect(tableLabel).toBeInTheDocument();

  const submitButton = await canvas.getByTestId('submit');

  // update fields
  const dbInput = await canvas.getByTestId('destination_database');
  userEvent.click(submitButton);

  expect(
    await canvas.findByText('Database is a required field!')
  ).toBeInTheDocument();

  userEvent.selectOptions(dbInput, 'chinook');

  const schemaInput = await canvas.getByTestId('destination_schema');
  userEvent.selectOptions(schemaInput, 'public');

  userEvent.click(submitButton);
  expect(
    await canvas.findByText('Table is a required field!')
  ).toBeInTheDocument();

  const tableInput = await canvas.getByTestId('destination_table');
  userEvent.selectOptions(tableInput, 'Album');

  // select a bigquery source
  userEvent.selectOptions(dbInput, 'bigquery_test');
  userEvent.selectOptions(schemaInput, 'sensei');
  userEvent.selectOptions(tableInput, 'table1');
};
