import { action } from '@storybook/addon-actions';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';

import { SimpleForm, InputField, useConsoleForm } from '.';
import { FiSearch } from 'react-icons/fi';
import { z } from 'zod';

export default {
  title: 'components/Forms 📁/InputField 🧬',
  component: InputField,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<input>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input)),
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<SimpleForm>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof InputField>;

export const ApiPlayground: StoryObj<typeof InputField> = {
  render: args => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField {...args} />
      </SimpleForm>
    );
  },

  name: '⚙️ API',

  args: {
    name: 'inputFieldName',
    label: 'Play with me!',
    placeholder: 'Play with me!',
  },
};

export const Basic: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField name="inputFieldName" label="The inputField label" />
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

export const VariantClearButton: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          clearButton
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

export const VariantEmailType: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          type="email"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Type email',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantPasswordType: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          type="password"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Type password',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantFileType: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          type="file"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Type file',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantWithDescription: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          description="InputField description"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - With description',
};

export const VariantWithTooltip: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          tooltip="InputField tooltip"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - With tooltip',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantSizeFull: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          size="full"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Size full',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantSizeMedium: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          size="medium"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Size medium',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantIconStart: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          icon={<FiSearch />}
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Icon start',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantIconEnd: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          icon={<FiSearch />}
          iconPosition="end"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Icon end',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantPrependLabel: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          prependLabel="Prepend label"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Prepend label',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantAppendLabel: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          appendLabel="Append label"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Append label',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantAutocompleteNewPassword: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="New password"
          placeholder="The new password"
          type="password"
          fieldProps={{ autoComplete: 'new-password' }}
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Autocomplete new pasword',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateWithDefaultValue: StoryObj<typeof InputField> = {
  render: () => {
    const defaultValues = { inputFieldName: 'value2' };

    const validationSchema = z.object({});

    return (
      <SimpleForm
        schema={validationSchema}
        options={{ defaultValues }}
        onSubmit={action('onSubmit')}
      >
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
        />
      </SimpleForm>
    );
  },

  name: '🔁 State - With default value',

  parameters: {
    docs: {
      description: {
        story: `Use \`<SimpleForm>\` options to set default value.`,
      },
      source: { state: 'open' },
    },
  },
};

export const StateLoading: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          loading
        />
      </SimpleForm>
    );
  },

  name: '🔁 State - Loading',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateDisabled: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          disabled
        />
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

export const StateWithErrorMessage: StoryObj<typeof InputField> = {
  render: () => {
    const schema = z.object({
      inputFieldName: z.enum(['value0', 'value1']),
    });

    const {
      methods: { trigger },
      Form,
    } = useConsoleForm({
      schema,
    });

    React.useEffect(() => {
      // Use useEffect hook to wait for the form to be rendered before triggering validation
      void trigger();
    });

    return (
      <Form onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
        />
      </Form>
    );
  },

  name: '🔁 State - With error message',

  parameters: {
    docs: {
      description: {
        story: `Incorrect value is set then \`<SimpleForm>\` validation is automatically triggered.`,
      },
      source: { state: 'open' },
    },
  },
};

export const TestingScalability: StoryObj<typeof InputField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <InputField
          name="inputFieldName"
          label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          placeholder="--Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.--"
          description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        />
      </SimpleForm>
    );
  },

  name: '🧪 Testing - Scalability',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
