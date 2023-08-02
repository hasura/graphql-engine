import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import React from 'react';
import { expect } from '@storybook/jest';
import { action } from '@storybook/addon-actions';
import { GiCarrot, GiShinyApple } from 'react-icons/gi';
import { FaCircle } from 'react-icons/fa';
import { z } from 'zod';
import { ListMap } from '.';
import { Button } from '../Button';
import { SimpleForm } from '../Form';

const schema = z.object({
  mapping: z.record(z.string()),
});

export default {
  title: 'components/ListMap',
  component: ListMap,
  decorators: [
    StoryComponent => {
      return (
        <SimpleForm
          options={{
            defaultValues: {
              mapping: { apple: 'tomato', cherry: 'chilli' },
            },
          }}
          onSubmit={action('onSubmit')}
          schema={schema}
        >
          <StoryComponent />
        </SimpleForm>
      );
    },
  ],
} as Meta<typeof ListMap>;

export const ArrayToArray: StoryFn = () => {
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

export const ArrayToString: StoryFn = () => {
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

export const WithIcons: StoryFn = () => {
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

export const WithMultipleIcons: StoryFn = () => {
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

export const WithNoLabelsAndNoBackground: StoryObj = {
  render: ({ className }) => {
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
  },

  args: {
    className: 'bg-transparent border-none',
  },
};

const FormElements = () => {
  return (
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
  );
};

export const WithReactFormHookNested: StoryObj = {
  render: () => {
    return (
      <>
        <FormElements />
        <Button type="submit" data-testid="submit">
          Submit
        </Button>
      </>
    );
  },

  play: async ({ canvasElement }) => {
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
  },
};
