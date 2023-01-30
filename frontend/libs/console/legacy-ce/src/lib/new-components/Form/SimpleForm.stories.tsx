import React from 'react';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { z } from 'zod';
import { InputField, SimpleForm } from '@/new-components/Form';
import { Button } from '@/new-components/Button';

export default {
  title: 'components/Forms 📁/Form 📁/Simple forms 🧬',
  component: SimpleForm,
  parameters: {
    docs: {
      description: {
        component: `A simple form component. For most advanced usage (set focus, trigeer validation, ...), use \`useConsoleForm\` hook instead.`,
      },
      source: { type: 'code', state: 'open' },
    },
  },
  decorators: [Story => <div className="p-4 ">{Story()}</div>],
} as ComponentMeta<typeof SimpleForm>;

export const Basic: ComponentStory<typeof SimpleForm> = () => {
  const validationSchema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  return (
    <SimpleForm
      // Apply validation schema to the form
      schema={validationSchema}
      onSubmit={action('onSubmit')}
    >
      <div className="space-y-xs">
        <h1 className="text-xl font-semibold mb-xs">Basic form</h1>
        <InputField
          name="inputFieldName"
          label="The input field label"
          placeholder="Input field placeholder"
          clearButton
        />
        <Button type="submit" mode="primary">
          Submit
        </Button>
      </div>
    </SimpleForm>
  );
};
Basic.storyName = '💠 Basic usage';
Basic.parameters = {
  docs: {
    description: {
      story: `\`<SimpleForm>\` component eases the task of forms creation.

It uses [**React Hook Form**](https://react-hook-form.com/) to
handle form validation and submission, validation schema is provided by [**Zod**](https://zod.dev/).

- 💡 Use the [**Storybook addon 'Actions'**](https://storybook.js.org/docs/react/essentials/actions) to see submitted values.`,
    },
  },
};

export const FormInputDefaultValue: ComponentStory<typeof SimpleForm> = () => {
  const validationSchema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  return (
    <SimpleForm
      schema={validationSchema}
      options={{
        defaultValues: {
          inputFieldName: 'Field defaut value',
        },
      }}
      onSubmit={action('onSubmit')}
    >
      <div className="space-y-xs">
        <h1 className="text-xl font-semibold mb-xs">Default value</h1>
        <InputField
          name="inputFieldName"
          label="The input field label"
          placeholder="Input field placeholder"
          clearButton
        />
        <Button type="submit" mode="primary">
          Submit
        </Button>
      </div>
    </SimpleForm>
  );
};
FormInputDefaultValue.storyName = '💠 Form input default value';
FormInputDefaultValue.parameters = {
  docs: {
    description: {
      story: `In this example, the form is automatically filled with the \`Hello world !\` 
value for the \`inputFieldName\` input.`,
    },
  },
};
