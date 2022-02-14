import React from 'react';
import { ComponentMeta } from '@storybook/react';
import { DevTool } from '@hookform/devtools';
import { z } from 'zod';
import { Select } from './Select';
import { Form } from '.';

const schema = z.object({
  value1: z.enum(['chocolate1', 'chocolate2', 'strawberry', 'vanilla']),
  value2: z.enum(['chocolate1', 'chocolate2', 'strawberry', 'vanilla']),
  value3: z.enum(['chocolate1', 'chocolate2', 'strawberry', 'vanilla']),
  value4: z.enum(['chocolate1', 'chocolate2', 'strawberry', 'vanilla']),
});

export default {
  title: 'components/Forms/Select',
  component: Select,
} as ComponentMeta<typeof Select>;

const options = [
  { value: 'chocolate1', label: 'Chocolate' },
  { value: 'chocolate2', label: 'Chocolate' },
  { value: 'strawberry', label: 'Strawberry' },
  { value: 'vanilla', label: 'Vanilla' },
];

export const Showcase = () => (
  <Form
    onSubmit={async values => {
      alert(JSON.stringify(values, null, 2));
    }}
    id="my-form"
    options={{
      defaultValues: {
        value3: 'vanilla',
        value4: 'strawberry',
      },
      mode: 'onBlur',
    }}
    schema={schema}
  >
    {({ control }) => (
      <div>
        <Select
          options={options}
          name="value1"
          label="Simple Select"
          tooltip="this is tooltip"
          description="this is a description"
        />
        <Select
          name="value2"
          options={options}
          placeholder="Please Select a Relationship..."
          label="With Placeholder"
          tooltip="this is tooltip"
          description="this is a description"
        />
        <Select
          name="value3"
          options={options}
          placeholder="Please Select a Relationship..."
          label="Pre-Selected Select Component"
          tooltip="this is tooltip"
          description="this is a description"
        />
        <Select
          name="value4"
          options={options}
          placeholder="Please Select a Relationship..."
          label="Pre-Selected Select Component with Placeholder"
          tooltip="this is tooltip"
          description="this is a description"
        />
        <button type="submit">Submit</button>
        <DevTool control={control} />
      </div>
    )}
  </Form>
);
