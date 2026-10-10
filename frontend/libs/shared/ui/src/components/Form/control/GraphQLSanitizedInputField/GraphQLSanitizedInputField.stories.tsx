import { action } from 'storybook/actions';
import { Meta, StoryFn, StoryObj } from '@storybook/react-webpack5';
import { GraphQLSanitizedInputField as InputField } from './index';
import { z } from 'zod';
import { SimpleForm } from '../../SimpleForm';

type StoryType = StoryFn<typeof InputField>;

export default {
  title: 'components/Forms 📁/control/GraphQLSanitizedInputField 🧬',
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

export const ApiPlayground: StoryObj<typeof InputField> = {
  render: (args) => {
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
    fieldProps: { placeholder: 'Try typing spaces and other stuff!' },
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
          fieldProps={{ placeholder: 'Try typing spaces and other stuff!' }}
        />
        <InputField
          name="sanitized-input-no-tips"
          label="No tips in description"
          fieldProps={{ placeholder: 'Try typing spaces and other stuff!' }}
          hideTips
        />
      </div>
    </SimpleForm>
  );
};
