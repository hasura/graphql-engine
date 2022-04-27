import { ComponentMeta, Story } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import React from 'react';
import { expect } from '@storybook/jest';
import { GiCarrot, GiShinyApple } from 'react-icons/gi';
import { FaCircle } from 'react-icons/fa';
import { z } from 'zod';
import { ListMap } from '.';
import { Button } from '../Button';
import { Form } from '../Form';

export default {
  title: 'components/ListMap',
  component: ListMap,
} as ComponentMeta<typeof ListMap>;

export const ArrayToArray: Story = () => {
  return (
    <ListMap
      value={{ apple: 'tomato', cherry: 'chilli' }}
      name="type_selector"
      onChange={e => console.log(e)}
      from={{
        options: ['apple', 'cherry', 'banana', 'pineapple', 'strawberry'],
        label: 'Source Column',
        placeholder: 'Select a fruit',
      }}
      to={{
        type: 'array',
        options: ['tomato', 'chilli', 'pumpkin', 'onion', 'carrot'],
        label: 'Reference Column',
        placeholder: 'Select a vegatable',
      }}
    />
  );
};

export const ArrayToString: Story = () => {
  return (
    <ListMap
      value={{ apple: 'tomato', cherry: 'chilli' }}
      name="type_selector"
      onChange={e => console.log(e)}
      from={{
        options: ['apple', 'cherry', 'banana', 'pineapple', 'strawberry'],
        label: 'Source Column',
      }}
      to={{
        type: 'string',
        label: 'Reference Column',
      }}
    />
  );
};

export const WithIcons: Story = () => {
  return (
    <ListMap
      value={{ apple: 'tomato', cherry: 'chilli' }}
      name="type_selector"
      onChange={e => console.log(e)}
      from={{
        options: ['apple', 'cherry', 'banana', 'pineapple', 'strawberry'],
        label: 'Source Column',
        icon: <GiShinyApple />,
      }}
      to={{
        type: 'array',
        options: ['tomato', 'chilli', 'pumpkin', 'onion', 'carrot'],
        label: 'Reference Column',
        icon: <GiCarrot />,
      }}
    />
  );
};

export const WithMultipleIcons: Story = () => {
  return (
    <ListMap
      value={{ apple: 'tomato', cherry: 'chilli' }}
      name="type_selector"
      onChange={e => console.log(e)}
      from={{
        options: ['apple', 'cherry', 'banana', 'pineapple', 'strawberry'],
        label: 'Source Column',
        icon: [<FaCircle />, <GiShinyApple />],
      }}
      to={{
        type: 'array',
        options: ['tomato', 'chilli', 'pumpkin', 'onion', 'carrot'],
        label: 'Reference Column',
        icon: [<FaCircle />, <GiShinyApple />],
      }}
    />
  );
};

export const WithNoLabelsAndNoBackground: Story = ({ className }) => {
  return (
    <ListMap
      value={{ apple: 'tomato', cherry: 'chilli' }}
      name="type_selector"
      onChange={e => console.log(e)}
      from={{
        options: ['apple', 'cherry', 'banana', 'pineapple', 'strawberry'],
      }}
      to={{
        type: 'array',
        options: ['tomato', 'chilli', 'pumpkin', 'onion', 'carrot'],
      }}
      className={className}
    />
  );
};

WithNoLabelsAndNoBackground.args = {
  className: 'bg-transparent border-none',
};

type Schema = z.infer<typeof schema>;

const schema = z.object({
  mapping: z.record(z.string()),
});

const FormElements = () => {
  return (
    <>
      <ListMap
        from={{
          options: ['apple', 'cherry', 'banana', 'pineapple', 'strawberry'],
          label: 'Source Column',
          placeholder: 'Select a fruit',
        }}
        to={{
          type: 'array',
          options: ['tomato', 'chilli', 'pumpkin', 'onion', 'carrot'],
          label: 'Reference Column',
          placeholder: 'Select a vegatable',
        }}
        name="mapping"
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

  const addNewRowBtn = await canvas.getByTestId('mapping_add_new_row');
  userEvent.click(addNewRowBtn);

  // // update fields
  const fromInput = await canvas.getByTestId('mapping_source_input_2');
  userEvent.selectOptions(fromInput, 'banana');

  const toInput = await canvas.getByTestId('mapping_target_input_2');
  userEvent.selectOptions(toInput, 'carrot');

  const submitButton = await canvas.getByTestId('submit');
  userEvent.click(submitButton);
};
