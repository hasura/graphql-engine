import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { z } from 'zod';
import { SimpleForm, Textarea, useConsoleForm } from '@/new-components/Form';

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
} as ComponentMeta<typeof Textarea>;

export const ApiPlayground: ComponentStory<typeof Textarea> = args => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <Textarea {...args} />
    </SimpleForm>
  );
};
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  name: 'textareaName',
  label: 'Play with me!',
};

export const Basic: ComponentStory<typeof Textarea> = () => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <Textarea name="textareaName" label="The textarea label" />
    </SimpleForm>
  );
};
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescription: ComponentStory<typeof Textarea> = () => {
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
};
VariantWithDescription.storyName = '🎭 Variant - With description';
VariantWithDescription.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithTooltip: ComponentStory<typeof Textarea> = () => {
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
};
VariantWithTooltip.storyName = '🎭 Variant - With tooltip';
VariantWithTooltip.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantSizeFull: ComponentStory<typeof Textarea> = () => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <Textarea name="textareaName" label="The textarea label" size="full" />
    </SimpleForm>
  );
};
VariantSizeFull.storyName = '🎭 Variant - Size full';
VariantSizeFull.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantSizeMedium: ComponentStory<typeof Textarea> = () => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <Textarea name="textareaName" label="The textarea label" size="medium" />
    </SimpleForm>
  );
};
VariantSizeMedium.storyName = '🎭 Variant - Size medium';
VariantSizeMedium.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateWithDefaultValue: ComponentStory<typeof Textarea> = () => {
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
};
StateWithDefaultValue.storyName = '🔁 State - With default value';
StateWithDefaultValue.parameters = {
  docs: {
    description: {
      story: `Use \`<SimpleForm>\` options to set default value.`,
    },
    source: { state: 'open' },
  },
};

export const StateLoading: ComponentStory<typeof Textarea> = () => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <Textarea name="textareaName" label="The textarea label" loading />
    </SimpleForm>
  );
};
StateLoading.storyName = '🔁 State - Loading';
StateLoading.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateDisabled: ComponentStory<typeof Textarea> = () => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <Textarea name="textareaName" label="The textarea label" disabled />
    </SimpleForm>
  );
};
StateDisabled.storyName = '🔁 State - Disabled';
StateDisabled.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateWithErrorMessage: ComponentStory<typeof Textarea> = () => {
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
    trigger();
  });

  return (
    <Form onSubmit={action('onSubmit')}>
      <Textarea name="textareaName" label="The textarea label" />
    </Form>
  );
};
StateWithErrorMessage.storyName = '🔁 State - With error message';
StateWithErrorMessage.parameters = {
  docs: {
    description: {
      story: `Incorrect value is set then \`<SimpleForm>\` validation is automatically triggered.`,
    },
    source: { state: 'open' },
  },
};

export const TestingScalability: ComponentStory<typeof Textarea> = () => {
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
};
TestingScalability.storyName = '🧪 Testing - Scalability';
TestingScalability.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
