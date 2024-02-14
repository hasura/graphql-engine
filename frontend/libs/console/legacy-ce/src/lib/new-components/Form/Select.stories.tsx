import React from 'react';
import type { Meta, StoryObj } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { z } from 'zod';
import { SimpleForm, Select, useConsoleForm } from '.';

const formDecorator =
  ({ triggerValidation = false, withDefaultValue = false } = {}) =>
  (Story: React.FC) => {
    const schema = z.object({
      selectNames: z
        .enum(['value0', 'value1', 'value2'])
        .refine(val => val !== 'value2', {
          message: 'Value2 not suitable for this case',
        }),
    });
    const defaultValues: z.infer<typeof schema> = { selectNames: 'value2' };

    const {
      methods: { trigger },
      Form,
    } = useConsoleForm({
      schema,
      ...(withDefaultValue ? { options: { defaultValues } } : {}),
    });

    React.useEffect(() => {
      if (triggerValidation) {
        void trigger();
      }
    }, []);

    return (
      <Form onSubmit={action('onSubmit')}>
        <Story />
      </Form>
    );
  };

export default {
  title: 'components/Forms 📁/SimpleSelectField 🧬',
  component: Select,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<select>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/select)),
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<SimpleForm>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} satisfies Meta<typeof Select>;

export const ApiPlayground: StoryObj<typeof Select> = {
  name: '⚙️ API',
  args: {
    name: 'selectNames',
    label: 'Play with me!',
    options: [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ],
  },
  decorators: [formDecorator()],
};

export const Basic: StoryObj<typeof Select> = {
  name: '🧰 Basic',
  args: {
    name: 'selectNames',
    label: 'Play with me!',
    options: [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', disabled: true },
      { value: 'value2', label: 'Value 2' },
    ],
  },
  decorators: [formDecorator()],
};

export const VariantWithDescription: StoryObj<typeof Select> = {
  ...Basic,
  name: '🎭 Variant - With description',
  args: {
    ...Basic.args,
    description: 'SimpleSelectField description',
  },
};

export const VariantWithTooltip: StoryObj<typeof Select> = {
  ...Basic,
  name: '🎭 Variant - With tooltip',
  args: {
    ...Basic.args,
    tooltip: 'SimpleSelectField tooltip',
  },
};

export const StateWithDefaultValue: StoryObj<typeof Select> = {
  ...Basic,
  name: '🔁 State - With default value',
  args: {
    ...Basic.args,
  },
  decorators: [formDecorator({ withDefaultValue: true })],
};

export const StateLoading: StoryObj<typeof Select> = {
  ...Basic,
  name: '🔁 State - Loading',
  args: {
    ...Basic.args,
    loading: true,
  },
};

export const StateDisabled: StoryObj<typeof Select> = {
  ...Basic,
  name: '🔁 State - Disabled',
  args: {
    ...Basic.args,
    disabled: true,
  },
};

export const StateWithErrorMessage: StoryObj<typeof Select> = {
  ...Basic,
  name: '🔁 State - With error message',
  decorators: [
    formDecorator({ triggerValidation: true, withDefaultValue: true }),
  ],
};

export const TestingScalability: StoryObj<typeof Select> = {
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
        <Select
          name="selectNames"
          label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          options={options}
        />
      </SimpleForm>
    );
  },
  name: '🧪 Testing - Scalability',
};
