import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { UseFormReturn } from 'react-hook-form';
import { z } from 'zod';
import { Form, InputField } from '@/new-components/Form';
import { FiSearch } from 'react-icons/fi';

export default {
  title: 'components/Forms 📁/InputField 🧬',
  component: InputField,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<input>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input)),
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<Form>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} as ComponentMeta<typeof InputField>;

export const ApiPlayground: ComponentStory<typeof InputField> = args => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => <InputField {...args} />}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => <InputField name="inputFieldName" label="The inputField label" />}
    </Form>
  );
};
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantEmailType: ComponentStory<typeof InputField> = () => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          type="email"
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          type="password"
        />
      )}
    </Form>
  );
};
VariantPasswordType.storyName = '🎭 Variant - Type password';
VariantPasswordType.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescription: ComponentStory<typeof InputField> = () => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          description="InputField description"
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          tooltip="InputField tooltip"
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          size="full"
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          size="medium"
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          icon={<FiSearch />}
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          icon={<FiSearch />}
          iconPosition="end"
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          prependLabel="Prepend label"
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          appendLabel="Append label"
        />
      )}
    </Form>
  );
};
VariantAppendLabel.storyName = '🎭 Variant - Append label';
VariantAppendLabel.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateWithDefaultValue: ComponentStory<typeof InputField> = () => {
  const defaultValues = { inputFieldName: 'value2' };

  const validationSchema = z.object({});

  return (
    <Form
      schema={validationSchema}
      options={{ defaultValues }}
      onSubmit={action('onSubmit')}
    >
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
        />
      )}
    </Form>
  );
};
StateWithDefaultValue.storyName = '🔁 State - With default value';
StateWithDefaultValue.parameters = {
  docs: {
    description: {
      story: `Use \`<Form>\` options to set default value.`,
    },
    source: { state: 'open' },
  },
};

export const StateLoading: ComponentStory<typeof InputField> = () => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          loading
        />
      )}
    </Form>
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
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
          disabled
        />
      )}
    </Form>
  );
};
StateDisabled.storyName = '🔁 State - Disabled';
StateDisabled.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateWithErrorMessage: ComponentStory<typeof InputField> = () => {
  const formRef = React.useRef<UseFormReturn>();

  React.useEffect(() => {
    formRef?.current?.trigger();
  });

  const validationSchema = z.object({
    inputFieldName: z.enum(['value0', 'value1']),
  });

  return (
    <Form ref={formRef} schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="The inputField label"
          placeholder="The inputField placeholder"
        />
      )}
    </Form>
  );
};
StateWithErrorMessage.storyName = '🔁 State - With error message';
StateWithErrorMessage.parameters = {
  docs: {
    description: {
      story: `Incorrect value is set then \`<Form>\` validation is automatically triggered.`,
    },
  },
};

export const TestingScalability: ComponentStory<typeof InputField> = () => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <InputField
          name="inputFieldName"
          label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          placeholder="--Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.--"
          description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        />
      )}
    </Form>
  );
};
TestingScalability.storyName = '🧪 Testing - Scalability';
TestingScalability.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
