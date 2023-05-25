import { action } from '@storybook/addon-actions';
import { StoryObj, Meta } from '@storybook/react';

import { z } from 'zod';
import { InputField, SimpleForm } from '.';
import { Button } from '../Button';
import { FormDebugWindow } from './dev-components/FormDebugWindow';

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
} as Meta<typeof SimpleForm>;

export const Basic: StoryObj<typeof SimpleForm> = {
  render: () => {
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
  },

  name: '💠 Basic usage',

  parameters: {
    docs: {
      description: {
        story: `\`<SimpleForm>\` component eases the task of forms creation.

  It uses [**React Hook Form**](https://react-hook-form.com/) to
  handle form validation and submission, validation schema is provided by [**Zod**](https://zod.dev/).

  - 💡 Use the [**Storybook addon 'Actions'**](https://storybook.js.org/docs/react/essentials/actions) to see submitted values.`,
      },
    },
  },
};

export const FormInputDefaultValue: StoryObj<typeof SimpleForm> = {
  render: () => {
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
  },

  name: '💠 Form input default value',

  parameters: {
    docs: {
      description: {
        story: `In this example, the form is automatically filled with the \`Hello world !\` 
  value for the \`inputFieldName\` input.`,
      },
    },
  },
};

export const FormDebug: StoryObj<typeof SimpleForm> = {
  render: () => {
    const validationSchema = z.object({
      inputFieldName: z.string().min(1),
    });

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')} debug>
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
  },

  name: '💠 Form Debugger',

  parameters: {
    docs: {
      description: {
        story: `In this example, the react dev tool component is rendered.`,
      },
    },
  },
};

export const FormWithDebugWindow: StoryObj<typeof SimpleForm> = {
  render: () => {
    const validationSchema = z.object({
      inputFieldName: z.string().min(1),
    });

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <FormDebugWindow />
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
  },

  name: '💠 Form With Debug Window',

  parameters: {
    docs: {
      description: {
        story: `In this example, a form debugger window is shown that displays form values and errors.`,
      },
    },
  },
};
