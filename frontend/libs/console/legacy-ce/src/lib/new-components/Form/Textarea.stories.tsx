import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { z } from 'zod';
import { SimpleForm, Textarea, useConsoleForm } from '.';

export default {
  title: 'components/Forms 📁/Textarea 🧬',
  component: Textarea,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<textarea>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea)),
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<SimpleForm>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof Textarea>;

export const ApiPlayground: StoryObj<typeof Textarea> = {
  render: args => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Textarea {...args} />
      </SimpleForm>
    );
  },

  name: '⚙️ API',

  args: {
    name: 'textareaName',
    label: 'Play with me!',
  },
};

export const Basic: StoryObj<typeof Textarea> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Textarea name="textareaName" label="The textarea label" />
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

export const VariantWithDescription: StoryObj<typeof Textarea> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Textarea
          name="textareaName"
          label="The textarea label"
          description="Textarea description"
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

export const VariantWithTooltip: StoryObj<typeof Textarea> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        {() => (
          <Textarea
            name="textareaName"
            label="The textarea label"
            tooltip="Textarea tooltip"
          />
        )}
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

export const VariantSizeFull: StoryObj<typeof Textarea> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Textarea name="textareaName" label="The textarea label" size="full" />
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

export const VariantSizeMedium: StoryObj<typeof Textarea> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Textarea
          name="textareaName"
          label="The textarea label"
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

export const StateWithDefaultValue: StoryObj<typeof Textarea> = {
  render: () => {
    const defaultValues = { textareaName: 'value2' };

    const validationSchema = z.object({});

    return (
      <SimpleForm
        schema={validationSchema}
        options={{ defaultValues }}
        onSubmit={action('onSubmit')}
      >
        <Textarea name="textareaName" label="The textarea label" />
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

export const StateLoading: StoryObj<typeof Textarea> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Textarea name="textareaName" label="The textarea label" loading />
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

export const StateDisabled: StoryObj<typeof Textarea> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Textarea name="textareaName" label="The textarea label" disabled />
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

export const StateWithErrorMessage: StoryObj<typeof Textarea> = {
  render: () => {
    const schema = z.object({
      textareaName: z.enum(['value0', 'value1']),
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
        <Textarea name="textareaName" label="The textarea label" />
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

export const TestingScalability: StoryObj<typeof Textarea> = {
  render: () => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Textarea
          name="textareaName"
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
