import React, { act } from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { action } from 'storybook/actions';
import { z } from 'zod';
import { CheckboxesField } from './index';
import { SimpleForm } from '../../SimpleForm';
import { useConsoleForm } from '../../hooks/useConsoleForm';

export default {
  title: 'components/Forms 📁/control/CheckboxesField 🧬',
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
} as Meta<typeof CheckboxesField>;

export const ApiPlayground: StoryObj<typeof CheckboxesField> = {
  render: (args) => {
    const validationSchema = z.object({
      checkboxNames: z.enum(['value0', 'value1', 'value2']),
    });

    return (
      <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
        <CheckboxesField {...args} />
      </SimpleForm>
    );
  },

  name: '⚙️ API',

  args: {
    name: 'checkboxNames',
    label: 'Play with me!',
    options: [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ],
  },
};

export const Basic: StoryObj<typeof CheckboxesField> = {
  render: () => {
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
  },

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantOrientation: StoryObj<typeof CheckboxesField> = {
  render: () => {
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
  },

  name: '🎭 Variant - Orientation',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantWithDescription: StoryObj<typeof CheckboxesField> = {
  render: () => {
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
  },

  name: '🎭 Variant - With description',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantWithTooltip: StoryObj<typeof CheckboxesField> = {
  render: () => {
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
  },

  name: '🎭 Variant - With tooltip',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateWithDefaultValue: StoryObj<typeof CheckboxesField> = {
  render: () => {
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
  },

  name: '🔁 State - With default value',

  parameters: {
    docs: {
      description: {
        story: `Use \`<Form>\` options to set default value.`,
      },
      source: { state: 'open' },
    },
  },
};

export const StateDisabled: StoryObj<typeof CheckboxesField> = {
  render: () => {
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
  },

  name: '🔁 State - Disabled',
};

export const StateWithErrorMessage: StoryObj<typeof CheckboxesField> = {
  render: () => {
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
      void act(() => {
        // Use useEffect hook to wait for the form to be rendered before triggering validation
        void trigger();
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
  },

  name: '🔁 State - With error message',

  parameters: {
    docs: {
      description: {
        story: `Incorrect value is set then \`<Form>\` validation is automatically triggered.`,
      },
      source: { state: 'open' },
    },
  },
};

export const TestingScalability: StoryObj<typeof CheckboxesField> = {
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
  },

  name: '🧪 Testing - Scalability',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
