import { ComponentMeta, Story } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import React from 'react';
import { Controller, useFormContext } from 'react-hook-form';
import { expect } from '@storybook/jest';
import { GiCarrot, GiShinyApple } from 'react-icons/gi';
import { z } from 'zod';
import { ListMap } from '.';
import { Button } from '../Button';
import { Form } from '../Form';

export default {
  title: 'components/ListMap',
  component: ListMap,
} as ComponentMeta<typeof ListMap>;

export const Playground: Story = () => {
  return (
    <ListMap
      fromLabel="Source Column"
      toLabel="Reference Column"
      maps={{ apple: 'tomato', cherry: 'chilli' }}
      fromOptions={['apple', 'cherry', 'banana', 'pineapple', 'strawberry']}
      toOptions={['tomato', 'chilli', 'pumpkin', 'onion', 'carrot']}
      name="type_selector"
      onChange={e => console.log(e)}
    />
  );
};

export const WithIcons: Story = () => {
  return (
    <ListMap
      fromLabel="Source Column"
      toLabel="Reference Column"
      maps={{ apple: 'tomato', cherry: 'chilli' }}
      fromOptions={['apple', 'cherry', 'banana', 'pineapple', 'strawberry']}
      toOptions={['tomato', 'chilli', 'pumpkin', 'onion', 'carrot']}
      name="type_selector"
      onChange={e => console.log(e)}
      fromIcon={<GiShinyApple />}
      toIcon={<GiCarrot />}
    />
  );
};

type Schema = z.infer<typeof schema>;

const schema = z.object({
  mapping: z.record(z.string()),
});

const FormElements = () => {
  const { control } = useFormContext<Schema>();

  return (
    <>
      <Controller
        control={control}
        name="mapping"
        render={({ field: { onChange, value } }) => (
          <ListMap
            onChange={onChange}
            fromLabel="Source Column"
            toLabel="Reference Column"
            maps={value}
            fromOptions={[
              'apple',
              'cherry',
              'banana',
              'pineapple',
              'strawberry',
            ]}
            toOptions={['tomato', 'chilli', 'pumpkin', 'onion', 'carrot']}
            name="mapping"
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
          mapping: { apple: 'tomato', cherry: 'chilli' },
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

  const fromLabel = await canvas.findByText('Source Column');
  const toLabel = await canvas.findByText('Reference Column');

  // expect error messages
  expect(fromLabel).toBeInTheDocument();
  expect(toLabel).toBeInTheDocument();

  // update fields
  const fromInput = await canvas.getByTestId('mapping_from_user_input');
  userEvent.selectOptions(fromInput, 'banana');

  const toInput = await canvas.getByTestId('mapping_to_user_input');
  userEvent.selectOptions(toInput, 'carrot');

  const submitButton = await canvas.getByTestId('submit');
  userEvent.click(submitButton);
};
