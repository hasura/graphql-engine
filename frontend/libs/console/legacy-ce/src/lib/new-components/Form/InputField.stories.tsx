import { action } from '@storybook/addon-actions';
import { ComponentMeta, ComponentStory } from '@storybook/react';
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
} as ComponentMeta<typeof InputField>;

export const ApiPlayground: ComponentStory<typeof InputField> = args => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <InputField {...args} />
    </SimpleForm>
  );
};
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  name: 'inputFieldName',
  label: 'Play with me!',
  placeholder: 'Play with me!',
};

export const Basic: ComponentStory<typeof InputField> = () => {
  const validationSchema = z.object({});

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <InputField name="inputFieldName" label="The inputField label" />
    </SimpleForm>
  );
};
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantClearButton: ComponentStory<typeof InputField> = () => {
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
};
VariantClearButton.storyName = '🎭 Variant - Clear button';
VariantClearButton.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantEmailType: ComponentStory<typeof InputField> = () => {
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
};
VariantEmailType.storyName = '🎭 Variant - Type email';
VariantEmailType.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantPasswordType: ComponentStory<typeof InputField> = () => {
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
};
VariantPasswordType.storyName = '🎭 Variant - Type password';
VariantPasswordType.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantFileType: ComponentStory<typeof InputField> = () => {
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
};
VariantFileType.storyName = '🎭 Variant - Type file';
VariantFileType.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescription: ComponentStory<typeof InputField> = () => {
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
};
VariantWithDescription.storyName = '🎭 Variant - With description';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithTooltip: ComponentStory<typeof InputField> = () => {
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
};
VariantWithTooltip.storyName = '🎭 Variant - With tooltip';
VariantWithTooltip.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantSizeFull: ComponentStory<typeof InputField> = () => {
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
};
VariantSizeFull.storyName = '🎭 Variant - Size full';
VariantSizeFull.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantSizeMedium: ComponentStory<typeof InputField> = () => {
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
};
VariantSizeMedium.storyName = '🎭 Variant - Size medium';
VariantSizeMedium.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantIconStart: ComponentStory<typeof InputField> = () => {
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
};
VariantIconStart.storyName = '🎭 Variant - Icon start';
VariantIconStart.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantIconEnd: ComponentStory<typeof InputField> = () => {
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
};
VariantIconEnd.storyName = '🎭 Variant - Icon end';
VariantIconEnd.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantPrependLabel: ComponentStory<typeof InputField> = () => {
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
};
VariantPrependLabel.storyName = '🎭 Variant - Prepend label';
VariantPrependLabel.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantAppendLabel: ComponentStory<typeof InputField> = () => {
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
};
VariantAppendLabel.storyName = '🎭 Variant - Append label';
VariantAppendLabel.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantAutocompleteNewPassword: ComponentStory<
  typeof InputField
> = () => {
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
};
VariantAutocompleteNewPassword.storyName =
  '🎭 Variant - Autocomplete new pasword';
VariantAutocompleteNewPassword.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateWithDefaultValue: ComponentStory<typeof InputField> = () => {
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

export const StateLoading: ComponentStory<typeof InputField> = () => {
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
};
StateLoading.storyName = '🔁 State - Loading';
StateLoading.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateDisabled: ComponentStory<typeof InputField> = () => {
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
};
StateDisabled.storyName = '🔁 State - Disabled';
StateDisabled.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateWithErrorMessage: ComponentStory<typeof InputField> = () => {
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

export const TestingScalability: ComponentStory<typeof InputField> = () => {
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
};
TestingScalability.storyName = '🧪 Testing - Scalability';
TestingScalability.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
