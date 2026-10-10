import { action } from 'storybook/actions';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { z } from 'zod';
import { FileInputField } from './index';
import { SimpleForm } from '../../SimpleForm';

export default {
  title: 'components/Forms 📁/control/FileInputField 🧬',
  component: FileInputField,
  parameters: {
    docs: {
      description: {
        component: `A file \`<input type="file">\` field wired into react-hook-form, sharing the same \`FieldWrapper\` (label, description, error) as \`InputField\`.`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof FileInputField>;

export const ApiPlayground: StoryObj<typeof FileInputField> = {
  render: (args) => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <FileInputField {...args} />
      </SimpleForm>
    );
  },

  name: '⚙️ API',

  args: {
    name: 'fileFieldName',
    label: 'Upload a file',
  },
};

export const Basic: StoryObj<typeof FileInputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <FileInputField name="fileFieldName" label="Upload a file" />
      </SimpleForm>
    );
  },

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantClearButton: StoryObj<typeof FileInputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <FileInputField
          name="fileFieldName"
          label="Upload a file"
          clearable
          onClear={action('onClear')}
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Clear button',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateDisabled: StoryObj<typeof FileInputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <FileInputField name="fileFieldName" label="Upload a file" disabled />
      </SimpleForm>
    );
  },

  name: '🔁 State - Disabled',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
