import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { z } from 'zod';
import { SimpleForm, Radio, useConsoleForm } from '.';

export default {
  title: 'components/Forms 📁/Radio 🧬',
  component: Radio,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<radio>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/radio)),
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<SimpleForm>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof Radio>;

export const ApiPlayground: StoryObj<typeof Radio> = {
  render: args => {
    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Radio {...args} />
      </SimpleForm>
    );
  },

  name: '⚙️ API',

  args: {
    name: 'radioNames',
    label: 'Play with me!',
    options: [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ],
  },
};

export const Basic: StoryObj<typeof Radio> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Radio name="radioNames" label="The radio label" options={options} />
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

export const VariantOrientation: StoryObj<typeof Radio> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <>
          <Radio name="radioNames" label="The radio label" options={options} />
          <Radio
            name="radioNames"
            label="The radio label"
            options={options}
            orientation="horizontal"
          />
        </>
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Orientation',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantWithDescription: StoryObj<typeof Radio> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Radio
          name="radioNames"
          label="The radio label"
          description="Radio description"
          options={options}
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

export const VariantWithTooltip: StoryObj<typeof Radio> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Radio
          name="radioNames"
          label="The radio label"
          tooltip="Radio tooltip"
          options={options}
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

export const StateWithDefaultValue: StoryObj<typeof Radio> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    const defaultValues = { radioNames: 'value2' };

    const validationSchema = z.object({});

    return (
      <SimpleForm
        schema={validationSchema}
        options={{ defaultValues }}
        onSubmit={action('onSubmit')}
      >
        <Radio name="radioNames" label="The radio label" options={options} />
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

export const StateLoading: StoryObj<typeof Radio> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Radio
          name="radioNames"
          label="The radio label"
          options={options}
          disabled
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

export const StateDisabled: StoryObj<typeof Radio> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ];

    const validationSchema = z.object({});

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Radio
          name="radioNames"
          label="The radio label"
          options={options}
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

export const StateWithErrorMessage: StoryObj<typeof Radio> = {
  render: () => {
    const options = [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', loading: true },
      { value: 'value2', label: 'Value 2' },
    ];

    const schema = z.object({
      radioNames: z.enum(['value0', 'value1']),
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
        <Radio name="radioNames" label="The radio label" options={options} />
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

export const TestingScalability: StoryObj<typeof Radio> = {
  render: () => {
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
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <Radio
          name="radioNames"
          label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          options={options}
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
