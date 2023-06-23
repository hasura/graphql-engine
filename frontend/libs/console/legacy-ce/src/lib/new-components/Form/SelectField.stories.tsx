import React from 'react';
import type { Meta, StoryObj } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { z } from 'zod';
import { SimpleForm, SelectField, useConsoleForm } from '.';
import { FaTable } from 'react-icons/fa';

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

const formDecoratorMulti =
  ({ triggerValidation = false, withDefaultValue = false } = {}) =>
  (Story: React.FC) => {
    const schema = z.object({
      selectNames: z
        .enum(['value0', 'value1', 'value2'])
        .array()
        .nonempty({
          message: 'Choose at least one option',
        })
        .refine(val => !val.includes('value2'), {
          message: 'Value2 not suitable for this case',
        }),
    });
    const defaultValues: z.infer<typeof schema> = {
      selectNames: ['value0', 'value1'],
    };

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
  title: 'components/Forms 📁/SelectField 🧬',
  component: SelectField,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping *react-select* component, see [react-select documentation](https://react-select.com/home) for more details, its label,
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<SimpleForm>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} satisfies Meta<typeof SelectField>;

export const ApiPlayground: StoryObj<typeof SelectField> = {
  name: '⚙️ API',
  args: {
    name: 'selectNames',
    label: 'Play with me!',
    options: [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', isDisabled: true },
      { value: 'value2', label: 'Value 2' },
    ],
  },
  decorators: [formDecorator()],
};

export const Basic: StoryObj<typeof SelectField> = {
  name: '🧰 Basic',
  args: {
    name: 'selectNames',
    label: 'Play with me!',
    options: [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', isDisabled: true },
      { value: 'value2', label: 'Value 2' },
    ],
  },
  decorators: [formDecorator()],
};

export const VariantWithDescription: StoryObj<typeof SelectField> = {
  ...Basic,
  name: '🎭 Variant - With description',
  args: {
    ...Basic.args,
    description: 'SelectField description',
  },
};

export const VariantWithTooltip: StoryObj<typeof SelectField> = {
  ...Basic,
  name: '🎭 Variant - With tooltip',
  args: {
    ...Basic.args,
    tooltip: 'SelectField tooltip',
  },
};

export const VariantWithIcon: StoryObj<typeof SelectField> = {
  ...Basic,
  name: '🎭 Variant - With Icon',
  args: {
    ...Basic.args,
    options: [
      {
        value: { name: 'Album', schema: 'public' },
        label: 'public / Album',
        icon: () => <FaTable />,
      },
      {
        value: { name: 'Artist', schema: 'public' },
        label: 'public / Artist',
        icon: () => <FaTable />,
      },
      {
        value: { name: 'Track', schema: 'public' },
        label: 'public / Track',
        icon: () => <FaTable />,
      },
      {
        value: { name: 'Genre', schema: 'public' },
        label: 'public / Genre',
        icon: () => <FaTable />,
      },
    ],
  },
};

export const StateWithDefaultValue: StoryObj<typeof SelectField> = {
  ...Basic,
  name: '🔁 State - With default value',
  args: {
    ...Basic.args,
  },
  decorators: [formDecorator({ withDefaultValue: true })],
};

export const StateGroupedWithDefaultValue: StoryObj<typeof SelectField> = {
  ...Basic,
  name: '🔁 State - Grouped with default value',
  args: {
    ...Basic.args,
    options: [
      {
        label: 'Group 1',
        options: [
          { value: 'value0', label: 'Value 0' },
          { value: 'value1', label: 'Value 1' },
          { value: 'value2', label: 'Value 2' },
        ],
      },
      {
        label: 'Group 2',
        options: [
          { value: 'value3', label: 'Value 3' },
          { value: 'value4', label: 'Value 4', isDisabled: true },
          { value: 'value5', label: 'Value 5' },
        ],
      },
    ],
  },
  decorators: [formDecorator({ withDefaultValue: true })],
};

export const StateLoading: StoryObj<typeof SelectField> = {
  ...Basic,
  name: '🔁 State - Loading',
  args: {
    ...Basic.args,
    loading: true,
  },
};

export const StateDisabled: StoryObj<typeof SelectField> = {
  ...Basic,
  name: '🔁 State - Disabled',
  args: {
    ...Basic.args,
    disabled: true,
  },
};

export const StateWithErrorMessage: StoryObj<typeof SelectField> = {
  ...Basic,
  name: '🔁 State - With error message',
  decorators: [
    formDecorator({ triggerValidation: true, withDefaultValue: true }),
  ],
};

export const TestingScalability: StoryObj<typeof SelectField> = {
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
        <SelectField
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

export const BasicMulti: StoryObj<typeof SelectField> = {
  name: '🧰 Basic multi',
  args: {
    name: 'selectNames',
    label: 'Play with me!',
    multi: true,
    options: [
      { value: 'value0', label: 'Value 0' },
      { value: 'value1', label: 'Value 1', isDisabled: true },
      { value: 'value2', label: 'Value 2' },
    ],
  },
  decorators: [formDecoratorMulti()],
};

export const StateWithDefaultValueMulti: StoryObj<typeof SelectField> = {
  ...BasicMulti,
  name: '🔁 State - Multi with default value',
  args: {
    ...BasicMulti.args,
    multi: true,
  },
  decorators: [formDecoratorMulti({ withDefaultValue: true })],
};

export const StateGroupedWithDefaultValueMulti: StoryObj<typeof SelectField> = {
  ...BasicMulti,
  name: '🔁 State - Multi grouped with default value',
  args: {
    ...BasicMulti.args,
    options: [
      {
        label: 'Group 1',
        options: [
          { value: 'value0', label: 'Value 0' },
          { value: 'value1', label: 'Value 1' },
          { value: 'value2', label: 'Value 2' },
        ],
      },
      {
        label: 'Group 2',
        options: [
          { value: 'value3', label: 'Value 3' },
          { value: 'value4', label: 'Value 4', isDisabled: true },
          { value: 'value5', label: 'Value 5' },
        ],
      },
    ],
  },
  decorators: [formDecoratorMulti({ withDefaultValue: true })],
};

const formDecoratorObject =
  ({ triggerValidation = false, withDefaultValue = false } = {}) =>
  (Story: React.FC) => {
    const schema = z.object({
      selectNames: z.object({ key: z.enum(['value0', 'value1', 'value2']) }),
    });
    const defaultValues: z.infer<typeof schema> = {
      selectNames: { key: 'value1' },
    };

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

export const BasicWithObjectAsValue: StoryObj<typeof SelectField> = {
  name: '🧰 Basic with object as default value',
  args: {
    name: 'selectNames',
    label: 'Play with me!',
    options: [
      { value: { key: 'value0' }, label: 'Value 0' },
      { value: { key: 'value1' }, label: 'Value 1' },
      { value: { key: 'value2' }, label: 'Value 2' },
    ],
  },
  decorators: [
    formDecoratorObject({ triggerValidation: true, withDefaultValue: true }),
  ],
};

const formDecoratorObjectMulti =
  ({ triggerValidation = false, withDefaultValue = false } = {}) =>
  (Story: React.FC) => {
    const schema = z.object({
      selectNames: z
        .array(z.object({ key: z.enum(['value0', 'value1', 'value2']) }))
        .nonempty({
          message: 'Choose at least one option',
        })
        .refine(val => !val.includes({ key: 'value2' }), {
          message: 'Value2 not suitable for this case',
        }),
    });
    const defaultValues: z.infer<typeof schema> = {
      selectNames: [{ key: 'value0' }, { key: 'value1' }],
    };

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

export const BasicMultiWithObjectAsValue: StoryObj<typeof SelectField> = {
  name: '🧰 Basic multi with object as default value',
  args: {
    name: 'selectNames',
    label: 'Play with me!',
    options: [
      {
        label: 'Group 1',
        options: [
          { value: { key: 'value0' }, label: 'Value 0' },
          { value: { key: 'value1' }, label: 'Value 1' },
          { value: { key: 'value2' }, label: 'Value 2' },
        ],
      },
      {
        label: 'Group 2',
        options: [
          { value: { key: 'value3' }, label: 'Value 3' },
          { value: { key: 'value4' }, label: 'Value 4', isDisabled: true },
          { value: { key: 'value5' }, label: 'Value 5' },
        ],
      },
    ],
    multi: true,
  },
  decorators: [
    formDecoratorObjectMulti({
      triggerValidation: true,
      withDefaultValue: true,
    }),
  ],
};
