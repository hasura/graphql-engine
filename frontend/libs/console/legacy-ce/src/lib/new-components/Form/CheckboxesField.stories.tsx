import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { act } from 'react-dom/test-utils';

import { z } from 'zod';
import {
  SimpleForm,
  CheckboxesField,
  useConsoleForm,
} from '@/new-components/Form';

export default {
  title: 'components/Forms 📁/CheckboxesField 🧬',
  component: CheckboxesField,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<checkbox>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/checkbox)),
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<Form>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} as ComponentMeta<typeof CheckboxesField>;

export const ApiPlayground: ComponentStory<typeof CheckboxesField> = args => {
  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1', 'value2']),
  });

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <CheckboxesField {...args} />
    </SimpleForm>
  );
};
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  name: 'checkboxNames',
  label: 'Play with me!',
  options: [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ],
};

export const Basic: ComponentStory<typeof CheckboxesField> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1', 'value2']),
  });

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <CheckboxesField
        name="checkboxNames"
        label="The checkbox label"
        options={options}
      />
    </SimpleForm>
  );
};
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantOrientation: ComponentStory<
  typeof CheckboxesField
> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1', 'value2']),
  });

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <>
        <CheckboxesField
          name="checkboxNames"
          label="The checkbox label"
          options={options}
        />
        <CheckboxesField
          name="checkboxNames"
          label="The checkbox label"
          options={options}
          orientation="horizontal"
        />
      </>
    </SimpleForm>
  );
};
VariantOrientation.storyName = '🎭 Variant - Orientation';
VariantOrientation.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescription: ComponentStory<
  typeof CheckboxesField
> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1', 'value2']),
  });

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <CheckboxesField
        name="checkboxNames"
        label="The checkbox label"
        description="CheckboxesField description"
        options={options}
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

export const VariantWithTooltip: ComponentStory<
  typeof CheckboxesField
> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1', 'value2']),
  });

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <CheckboxesField
        name="checkboxNames"
        label="The checkbox label"
        tooltip="CheckboxesField tooltip"
        options={options}
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

export const StateWithDefaultValue: ComponentStory<
  typeof CheckboxesField
> = () => {
  const options: Array<{
    value: 'value0' | 'value1' | 'value2';
    label: string;
    disabled?: boolean;
  }> = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1', 'value2']).array(),
  });

  const defaultValues: z.infer<typeof validationSchema> = {
    checkboxNames: ['value2'],
  };

  return (
    <SimpleForm
      schema={validationSchema}
      options={{ defaultValues }}
      onSubmit={action('onSubmit')}
    >
      <CheckboxesField
        name="checkboxNames"
        label="The checkbox label"
        options={options}
      />
    </SimpleForm>
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

export const StateDisabled: ComponentStory<typeof CheckboxesField> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1', 'value2']),
  });

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <CheckboxesField
        name="checkboxNames"
        label="The checkbox label"
        options={options}
        disabled
      />
    </SimpleForm>
  );
};
StateDisabled.storyName = '🔁 State - Disabled';

export const StateWithErrorMessage: ComponentStory<
  typeof CheckboxesField
> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1']),
  });

  const {
    methods: { trigger },
    Form,
  } = useConsoleForm({
    schema: validationSchema,
  });

  React.useEffect(() => {
    act(() => {
      // Use useEffect hook to wait for the form to be rendered before triggering validation
      trigger();
    });
  }, [trigger]);

  return (
    <Form onSubmit={action('onSubmit')}>
      <CheckboxesField
        name="checkboxNames"
        label="The checkbox label"
        options={options}
      />
    </Form>
  );
};
StateWithErrorMessage.storyName = '🔁 State - With error message';
StateWithErrorMessage.parameters = {
  docs: {
    description: {
      story: `Incorrect value is set then \`<Form>\` validation is automatically triggered.`,
    },
    source: { state: 'open' },
  },
};

export const TestingScalability: ComponentStory<
  typeof CheckboxesField
> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
    {
      value: 'value3',
      label:
        'Value 4 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
    },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1', 'value2']),
  });

  return (
    <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
      <CheckboxesField
        name="checkboxNames"
        label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        options={options}
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
