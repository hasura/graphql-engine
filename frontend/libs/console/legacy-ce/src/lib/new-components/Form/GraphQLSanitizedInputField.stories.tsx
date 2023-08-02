import { action } from '@storybook/addon-actions';
import { Meta, StoryFn } from '@storybook/react';
import React from 'react';

import { SimpleForm, GraphQLSanitizedInputField as InputField } from '.';
import { z } from 'zod';

type StoryType = StoryFn<typeof InputField>;

export default {
  title: 'components/Forms 📁/GraphQLSanitizedInputField 🧬',
  component: InputField,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping <InputField /> that sanitizes invalid GraphQL field characaters`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof InputField>;

export const ApiPlayground: StoryType = {
  render: args => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField {...args} />
      </SimpleForm>
    );
  },

  args: {
    name: 'input',
    label: 'With tips in description',
    placeholder: 'Try typing spaces and other stuff!',
    hideTips: false,
  },

  name: '⚙️ API',
};

export const Examples: StoryType = () => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <div className="max-w-xs">
        <InputField
          name="sanitized-input"
          label="With tips in description"
          placeholder="Try typing spaces and other stuff!"
        />
        <InputField
          name="sanitized-input-no-tips"
          label="No tips in description"
          placeholder="Try typing spaces and other stuff!"
          hideTips
        />
      </div>
    </SimpleForm>
  );
};
