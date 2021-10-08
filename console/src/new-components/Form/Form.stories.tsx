import React from 'react';
import { DevTool } from '@hookform/devtools';
import { FaSearch, FaAt } from 'react-icons/fa';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { z } from 'zod';
import { Form } from './Form';
import { InputField } from './InputField';

const schema = z.object({
  title: z.string().nonempty(),
  searchLeft: z.string().nonempty(),
  searchRight: z.string().nonempty(),
  email: z.string().email(),
  titleMedium: z.string().nonempty(),
  searchLeftMedium: z.string().nonempty(),
  searchRightMedium: z.string().nonempty(),
  emailMedium: z.string().email(),
});

export default {
  title: 'components/Forms/Form',
  component: Form,
  subcomponents: {
    input: InputField,
  },
} as ComponentMeta<typeof Form>;

export const Example: ComponentStory<typeof Form> = () => {
  return (
    <Form
      onSubmit={async values => {
        alert(JSON.stringify(values, null, 2));
      }}
      id="my-form"
      options={{
        defaultValues: {
          title: 'This is a default value',
        },
        mode: 'onBlur',
      }}
      schema={schema}
    >
      {({ control }) => (
        <div className="space-y-md">
          <h2 className="text-lg font-semibold">Size = Full</h2>
          <InputField
            label="Title"
            name="title"
            description="Some description"
            tooltip="And with tooltip"
          />
          <InputField
            label="Search left"
            name="searchLeft"
            icon={<FaSearch />}
          />
          <InputField
            label="Search right"
            name="searchRight"
            placeholder="This is a placeholder"
            icon={<FaSearch />}
            iconPosition="end"
          />
          <InputField label="Email" name="email" icon={<FaAt />} />
          <h2 className="text-lg font-semibold">Size = Medium</h2>
          <InputField
            label="Title"
            name="titleMedium"
            description="Some description"
            tooltip="And with tooltip"
            size="medium"
          />
          <InputField
            label="Search left disabled"
            name="searchLeftMedium"
            size="medium"
            disabled
            icon={<FaSearch />}
          />
          <InputField
            label="Search right"
            name="searchRightMedium"
            icon={<FaSearch />}
            iconPosition="end"
            size="medium"
          />
          <InputField
            label="Email"
            name="emailMedium"
            size="medium"
            icon={<FaAt />}
          />
          <button type="submit">Submit</button>
          <DevTool control={control} />
        </div>
      )}
    </Form>
  );
};
