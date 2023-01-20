import React from 'react';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { z } from 'zod';
import { DevTool } from '@hookform/devtools';

import {
  CheckboxesField,
  CodeEditorField,
  InputField,
  Radio,
  Select,
  Textarea,
  useConsoleForm,
} from '@/new-components/Form';
import { Button } from '@/new-components/Button';

export default {
  title: 'components/Forms 📁/Form 📁/Advanced forms with hook 🪝',
  parameters: {
    docs: {
      description: {
        component: `A hook exposing React Form Hook with Console form fields.

This is the advocated way to create forms with advanced features as it allows to access all the React Hook Form data and methods.

For simple use cases, use the \`<SimpleForm />\` component instead.`,
      },
      source: { type: 'code', state: 'open' },
    },
  },
  decorators: [Story => <div className="p-4 ">{Story()}</div>],
} as ComponentMeta<any>;

export const Basic: ComponentStory<any> = () => {
  const schema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  const {
    methods: { control },
    Form,
  } = useConsoleForm({
    schema,
  });

  return (
    <Form onSubmit={action('onSubmit')}>
      <div className="space-y-xs">
        <h1 className="text-xl font-semibold mb-xs">Basic form</h1>
        <InputField
          name="inputFieldName"
          label="The input field label"
          placeholder="Input field placeholder"
          clearButton
        />
        <Button type="submit" mode="primary">
          Submit
        </Button>
        {/* Debug form state */}
        <DevTool control={control} />
      </div>
    </Form>
  );
};
Basic.storyName = '💠 Basic usage';
Basic.parameters = {
  docs: {},
};

export const FormInputDefaultValue: ComponentStory<any> = () => {
  const schema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  const {
    methods: { control },
    Form,
  } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        inputFieldName: 'Input field default value',
      },
    },
  });

  return (
    <Form onSubmit={action('onSubmit')}>
      <div className="space-y-xs">
        <h1 className="text-xl font-semibold mb-xs">Basic form</h1>
        <InputField
          name="inputFieldName"
          label="The input field label"
          placeholder="Input field placeholder"
          clearButton
        />
        <Button type="submit" mode="primary">
          Submit
        </Button>
        {/* Debug form state */}
        <DevTool control={control} />
      </div>
    </Form>
  );
};
FormInputDefaultValue.storyName = '💠 With default value';
FormInputDefaultValue.parameters = {
  docs: {},
};

export const ManuallyTriggerFormValidation: ComponentStory<any> = () => {
  const schema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  const {
    methods: { trigger, control },
    Form,
  } = useConsoleForm({
    schema,
  });

  React.useEffect(() => {
    // Use useEffect hook to wait for the form to be rendered before triggering validation
    trigger();
  }, []);

  return (
    <Form onSubmit={action('onSubmit')}>
      <div className="space-y-xs">
        <h1 className="text-xl font-semibold mb-xs">Basic form</h1>
        <InputField
          name="inputFieldName"
          label="The input field label"
          placeholder="Input field placeholder"
          clearButton
        />
        <Button type="submit" mode="primary">
          Submit
        </Button>
        {/* Debug form state */}
        <DevTool control={control} />
      </div>
    </Form>
  );
};
ManuallyTriggerFormValidation.storyName = '💠 Manually trigger form validation';
ManuallyTriggerFormValidation.parameters = {
  docs: {},
};

export const ManuallyFocusField: ComponentStory<any> = () => {
  const schema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  const {
    methods: { setFocus, control },
    Form,
  } = useConsoleForm({
    schema,
  });

  React.useEffect(() => {
    setFocus('inputFieldName');
  }, []);

  return (
    <Form onSubmit={action('onSubmit')}>
      <div className="space-y-xs">
        <h1 className="text-xl font-semibold mb-xs">Basic form</h1>
        <InputField
          name="inputFieldName"
          label="The input field label"
          placeholder="Input field placeholder"
          clearButton
        />
        <Button type="submit" mode="primary">
          Submit
        </Button>
        {/* Debug form state */}
        <DevTool control={control} />
      </div>
    </Form>
  );
};
ManuallyFocusField.storyName = '💠 Manually trigger focus';
ManuallyFocusField.parameters = {
  docs: {},
};

export const AllInputs: ComponentStory<any> = () => {
  const schema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
    textareaName: z.string().min(1, { message: 'Mandatory field' }),
    selectName: z.string().min(1, { message: 'Mandatory field' }),
    checkboxesFieldNames: z
      .enum(['checkboxValue0', 'checkboxValue1', 'checkboxValue2'])
      .array()
      .nonempty({
        message: 'Choose at least one option',
      }),
    radioName: z
      // When nothing is selected, the value is null
      .union([z.string(), z.null()])
      .refine(
        value => typeof value === 'string' && value.length > 0,
        'Choose one option'
      ),
    codeEditorFieldName: z.string().min(1, { message: 'Mandatory field' }),
    inputFieldUploadName: z
      .any()
      .refine(files => files?.length === 1, 'File is required.'),
  });

  const {
    methods: { setFocus, reset, control },
    Form,
  } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        inputFieldName: 'Input field default value',
      },
    },
  });

  React.useEffect(() => {
    // Use useEffect hook to wait for the form to be rendered before triggering focus
    setFocus('textareaName');
  }, []);

  return (
    <Form onSubmit={action('onSubmit')}>
      <div className="space-y-xs">
        <h1 className="text-xl font-semibold mb-xs">Form title</h1>
        <InputField
          name="inputFieldName"
          label="The input field label"
          description="The input field description"
          tooltip="The input field tooltip"
          placeholder="Input field placeholder"
        />
        <Textarea
          name="textareaName"
          label="The textarea label"
          description="The textarea description"
          tooltip="The textarea tooltip"
          placeholder="Textarea field placeholder"
        />
        <Select
          name="selectName"
          options={[
            { value: 'selectValue0', label: 'Select value 0' },
            {
              value: 'selectValue1',
              label: 'Select value 1',
              disabled: true,
            },
            { value: 'selectValue2', label: 'Select value 2' },
          ]}
          label="The select label *"
          description="The select description"
          tooltip="The select tooltip"
          placeholder="--Select placeholder--"
        />
        <CheckboxesField
          name="checkboxesFieldNames"
          label="The checkbox label *"
          description="The checkbox description"
          tooltip="The checkbox tooltip"
          options={[
            { value: 'checkboxValue0', label: 'CheckboxesField value 0' },
            {
              value: 'checkboxValue1',
              label: 'CheckboxesField value 1',
              disabled: true,
            },
            { value: 'checkboxValue2', label: 'CheckboxesField value 2' },
          ]}
          orientation="horizontal"
        />
        <Radio
          name="radioName"
          label="The radio label *"
          description="The radio description"
          tooltip="The radio tooltip"
          options={[
            { value: 'radioValue0', label: 'Radio value 0' },
            {
              value: 'radioValue1',
              label: 'Radio value 1',
              disabled: true,
            },
            { value: 'radioValue2', label: 'Radio value 2' },
          ]}
          orientation="horizontal"
        />
        <CodeEditorField
          name="codeEditorFieldName"
          label="The code editor label *"
          description="The code editor description"
          tooltip="The code editor tooltip"
        />
        <InputField
          name="inputFieldUploadName"
          label="The upload input field label"
          description="The upload input field description"
          tooltip="The input upload field tooltip"
          placeholder="Input upload field placeholder"
          type="file"
        />
        <div className="flex gap-4">
          <Button type="button" onClick={() => reset({})}>
            Reset
          </Button>
          <Button type="submit" mode="primary">
            Submit
          </Button>
        </div>
        {/* Debug form state */}
        <DevTool control={control} />
      </div>
    </Form>
  );
};
AllInputs.storyName = '💠 Demo with all inputs and form reset';
AllInputs.parameters = {
  docs: {},
};
