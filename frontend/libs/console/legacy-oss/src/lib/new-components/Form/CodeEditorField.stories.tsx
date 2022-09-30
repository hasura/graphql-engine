import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { UseFormReturn } from 'react-hook-form';
import { z } from 'zod';
import { Form, CodeEditorField } from '@/new-components/Form';

export default {
  title: 'components/Forms 📁/CodeEditorField 🧬',
  component: CodeEditorField,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping an Ace editor ([see Docs](https://ace.c9.io/)),<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<Form>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
} as ComponentMeta<typeof CodeEditorField>;

export const ApiPlayground: ComponentStory<typeof CodeEditorField> = args => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => <CodeEditorField {...args} />}
    </Form>
  );
};
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  name: 'codeEditorFieldName',
  label: 'Play with me!',
};

export const Basic: ComponentStory<typeof CodeEditorField> = () => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
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

export const VariantWithDescription: ComponentStory<typeof CodeEditorField> =
  () => {
    const validationSchema = z.object({});

    return (
      <Form schema={validationSchema} onSubmit={action('onSubmit')}>
        {() => (
          <CodeEditorField
            name="codeEditorFieldName"
            label="The codeEditor label"
            description="CodeEditorField description"
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

export const VariantWithTooltip: ComponentStory<typeof CodeEditorField> =
  () => {
    const validationSchema = z.object({});

    return (
      <Form schema={validationSchema} onSubmit={action('onSubmit')}>
        {() => (
          <CodeEditorField
            name="codeEditorFieldName"
            label="The codeEditor label"
            tooltip="CodeEditorField tooltip"
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

export const VariantSizeFull: ComponentStory<typeof CodeEditorField> = () => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
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

export const VariantSizeMedium: ComponentStory<typeof CodeEditorField> = () => {
  const validationSchema = z.object({});

  return (
    <Form schema={validationSchema} onSubmit={action('onSubmit')}>
      {() => (
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
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

export const StateWithDefaultValue: ComponentStory<typeof CodeEditorField> =
  () => {
    const defaultValues = { codeEditorFieldName: '{ "prop": "value"}' };

    const validationSchema = z.object({});

    return (
      <Form
        schema={validationSchema}
        options={{ defaultValues }}
        onSubmit={action('onSubmit')}
      >
        {() => (
          <CodeEditorField
            name="codeEditorFieldName"
            label="The codeEditor label"
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

export const StateDisabled: ComponentStory<typeof CodeEditorField> = () => {
  const defaultValues = { codeEditorFieldName: '{ "prop": "value"}' };

  const validationSchema = z.object({});

  return (
    <Form
      schema={validationSchema}
      options={{ defaultValues }}
      onSubmit={action('onSubmit')}
    >
      {() => (
        <CodeEditorField
          name="codeEditorFieldName"
          label="The codeEditor label"
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

export const StateWithErrorMessage: ComponentStory<typeof CodeEditorField> =
  () => {
    const formRef = React.useRef<UseFormReturn>();

    React.useEffect(() => {
      formRef?.current?.trigger();
    });

    const validationSchema = z.object({
      codeEditorFieldName: z.enum(['value0', 'value1']),
    });

    return (
      <Form
        ref={formRef}
        schema={validationSchema}
        onSubmit={action('onSubmit')}
      >
        {() => (
          <CodeEditorField
            name="codeEditorFieldName"
            label="The codeEditor label"
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

export const TestingScalability: ComponentStory<typeof CodeEditorField> =
  () => {
    const validationSchema = z.object({});

    return (
      <Form schema={validationSchema} onSubmit={action('onSubmit')}>
        {() => (
          <CodeEditorField
            name="codeEditorFieldName"
            label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
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
