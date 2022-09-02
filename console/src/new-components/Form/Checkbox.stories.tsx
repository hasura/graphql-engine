import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { UseFormReturn } from 'react-hook-form';
import { z } from 'zod';
import { Form, Checkbox } from '@/new-components/Form';

export default {
  title: 'components/Forms 📁/Checkbox 🧬',
  component: Checkbox,
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
} as ComponentMeta<typeof Checkbox>;

export const ApiPlayground: ComponentStory<typeof Checkbox> = args => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => <Checkbox {...args} />}
    </Form>
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

export const Basic: ComponentStory<typeof Checkbox> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <Checkbox
          name="checkboxNames"
          label="The checkbox label"
          options={options}
        />
      )}
    </Form>
  );
};
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantOrientation: ComponentStory<typeof Checkbox> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <>
          <Checkbox
            name="checkboxNames"
            label="The checkbox label"
            options={options}
          />
          <Checkbox
            name="checkboxNames"
            label="The checkbox label"
            options={options}
            orientation="horizontal"
          />
        </>
      )}
    </Form>
  );
};
VariantOrientation.storyName = '🎭 Variant - Orientation';
VariantOrientation.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescription: ComponentStory<typeof Checkbox> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <Checkbox
          name="checkboxNames"
          label="The checkbox label"
          description="Checkbox description"
          options={options}
        />
      )}
    </Form>
  );
};
VariantWithDescription.storyName = '🎭 Variant - With description';
VariantWithDescription.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithTooltip: ComponentStory<typeof Checkbox> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <Checkbox
          name="checkboxNames"
          label="The checkbox label"
          tooltip="Checkbox tooltip"
          options={options}
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

export const StateWithDefaultValue: ComponentStory<typeof Checkbox> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const defaultValues = { checkboxNames: 'value2' };

  const validationSchema = z.object({});

  return (
    <Form
      schema={validationSchema}
      options={{ defaultValues }}
      onSubmit={action('onSubmit')}
    >
      {() => (
        <Checkbox
          name="checkboxNames"
          label="The checkbox label"
          options={options}
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

export const StateDisabled: ComponentStory<typeof Checkbox> = () => {
  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <Checkbox
          name="checkboxNames"
          label="The checkbox label"
          options={options}
          disabled
        />
      )}
    </Form>
  );
};
StateDisabled.storyName = '🔁 State - Disabled';

export const StateWithErrorMessage: ComponentStory<typeof Checkbox> = () => {
  const formRef = React.useRef<UseFormReturn>();

  React.useEffect(() => {
    formRef?.current?.trigger();
  });

  const options = [
    { value: 'value0', label: 'Value 0' },
    { value: 'value1', label: 'Value 1', disabled: true },
    { value: 'value2', label: 'Value 2' },
  ];

  const validationSchema = z.object({
    checkboxNames: z.enum(['value0', 'value1']),
  });

  return (
    <Form ref={formRef} schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <Checkbox
          name="checkboxNames"
          label="The checkbox label"
          options={options}
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
    source: { state: 'open' },
  },
};

export const TestingScalability: ComponentStory<typeof Checkbox> = () => {
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

  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <Checkbox
          name="checkboxNames"
          label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          options={options}
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
