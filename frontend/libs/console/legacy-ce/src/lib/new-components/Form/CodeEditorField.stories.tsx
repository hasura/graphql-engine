import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { act } from 'react-dom/test-utils';

import { z } from 'zod';
import { SimpleForm, CodeEditorField, useConsoleForm } from '.';

export default {
  title: 'components/Forms 📁/CodeEditorField 🧬',
  component: CodeEditorField,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping an Ace editor ([see Docs](https://ace.c9.io/)),<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<SimpleForm>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof CodeEditorField>;

export const ApiPlayground: StoryObj<typeof CodeEditorField> = {
  render: args => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <CodeEditorField {...args} />
      </SimpleForm>
    );
  },

  name: '⚙️ API',

  args: {
    name: 'codeEditorFieldName',
    label: 'Play with me!',
    placeholder: 'CodeEditorField placeholder',
  },
};

export const Basic: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
        />
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

export const VariantWithDescription: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
          description="CodeEditorField description"
          placeholder="CodeEditorField placeholder"
        />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - With description',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantWithTooltip: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
          tooltip="CodeEditorField tooltip"
          placeholder="CodeEditorField placeholder"
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

export const VariantSizeFull: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
          placeholder="CodeEditorField placeholder"
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

export const VariantSizeMedium: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
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

export const StateWithDefaultValue: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const defaultValues = { codeEditorFieldName: '{ "prop": "value"}' };

    const validationSchema = z.object({});

    return (
      <SimpleForm
        schema={validationSchema}
        options={{ defaultValues }}
        onSubmit={action('onSubmit')}
      >
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
          placeholder="CodeEditorField placeholder"
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

export const StateLoading: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const defaultValues = { codeEditorFieldName: '{ "prop": "value"}' };

    const validationSchema = z.object({});

    return (
      <SimpleForm
        schema={validationSchema}
        options={{ defaultValues }}
        onSubmit={action('onSubmit')}
      >
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
          placeholder="CodeEditorField placeholder"
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

export const StateDisabled: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const defaultValues = { codeEditorFieldName: '{ "prop": "value"}' };

    const validationSchema = z.object({});

    return (
      <SimpleForm
        schema={validationSchema}
        options={{ defaultValues }}
        onSubmit={action('onSubmit')}
      >
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
          placeholder="CodeEditorField placeholder"
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

export const StateWithErrorMessage: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const schema = z.object({
      codeEditorFieldName: z.enum(['value0', 'value1']),
    });

    const {
      methods: { trigger },
      Form,
    } = useConsoleForm({
      schema,
    });

    React.useEffect(() => {
      void act(() => {
        // Use useEffect hook to wait for the form to be rendered before triggering validation
        void trigger();
      });
    }, [trigger]);

    return (
      <Form onSubmit={action('onSubmit')}>
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
          placeholder="CodeEditorField placeholder"
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

export const TestingScalability: StoryObj<typeof CodeEditorField> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <CodeEditorField
          name="codeEditorFieldName"
          label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          placeholder="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
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
