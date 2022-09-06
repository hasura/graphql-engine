import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { UseFormReturn } from 'react-hook-form';
import { DevTool } from '@hookform/devtools';
import { z } from 'zod';
import {
  Form,
  InputField,
  Textarea,
  Select,
  Checkbox,
  Radio,
  CodeEditorField,
} from '@/new-components/Form';
import { Button } from '@/new-components/Button';

export default {
  title: 'components/Forms 📁/Form 🦠',
  component: Form,
  parameters: {
    docs: {
      description: {
        component: `A component wrapping native \`<checkbox>\` element ([see MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/checkbox)),
its description, hint and error message.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<Form>\` padding).`,
      },
      source: { type: 'code', state: 'open' },
    },
  },
} as ComponentMeta<typeof Form>;

export const Basic: ComponentStory<typeof Form> = () => {
  const validationSchema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  return (
    <Form
      // Apply validation schema to the form
      schema={validationSchema}
      onSubmit={action('onSubmit')}
    >
      {/* 💡 The `control` prop is provided by [**React Hook Form**](https://react-hook-form.com/api/useform)
      and is used to access the form state. */}
      {({ control }) => (
        <div className="space-y-2">
          <h1 className="text-xl font-semibold mb-xs">Basic form</h1>
          <InputField
            name="inputFieldName"
            label="The input field label"
            placeholder="Input field placeholder"
          />
          <Button type="submit" mode="primary">
            Submit
          </Button>
          {/* Debug form state */}
          <DevTool control={control} />
        </div>
      )}
    </Form>
  );
};
Basic.storyName = '💠 Basic usage';
Basic.parameters = {
  docs: {
    description: {
      story: `\`<Form>\` component eases the task of forms creation.

It uses [**React Hook Form**](https://react-hook-form.com/) to
handle form validation and submission, validation schema is provided by [**Zod**](https://zod.dev/).

- 💡 Use the [\`<DevTool>\` component](https://react-hook-form.com/dev-tools) to debug the form state.
- 💡 Use the [**Storybook addon 'Actions'**](https://storybook.js.org/docs/react/essentials/actions) to see submitted values.`,
    },
  },
};

export const FormInputDefaultValue: ComponentStory<typeof Form> = () => {
  const validationSchema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  return (
    <Form
      schema={validationSchema}
      options={{
        defaultValues: {
          inputFieldName: 'Hello world !',
        },
      }}
      onSubmit={action('onSubmit')}
    >
      {({ control }) => (
        <div className="space-y-2">
          <h1 className="text-xl font-semibold mb-xs">Default value</h1>
          <InputField
            name="inputFieldName"
            label="The input field label"
            placeholder="Input field placeholder"
          />
          <Button type="submit" mode="primary">
            Submit
          </Button>
          <DevTool control={control} />
        </div>
      )}
    </Form>
  );
};
FormInputDefaultValue.storyName = '💠 Form input default value';
FormInputDefaultValue.parameters = {
  docs: {
    description: {
      story: `In this example, the form is automatically filled with the \`Hello world !\` 
value for the \`inputFieldName\` input.`,
    },
  },
};

export const ManuallyTriggerFormValidation: ComponentStory<typeof Form> =
  () => {
    const validationSchema = z.object({
      inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
    });

    const formRef = React.useRef<UseFormReturn>();
    React.useEffect(() => {
      // Use useEffect hook to wait for the form to be rendered before triggering validation
      formRef?.current?.trigger();
    });

    return (
      <Form
        id="formId"
        ref={formRef}
        schema={validationSchema}
        onSubmit={action('onSubmit')}
      >
        {({ control }) => (
          <div className="space-y-2">
            <h1 className="text-xl font-semibold mb-xs">
              Manually trigger form validation
            </h1>
            <InputField
              name="inputFieldName"
              label="The input field label"
              placeholder="Input field placeholder"
            />
            <Button type="submit" mode="primary">
              Submit
            </Button>
            {/* Debug form state */}
            <DevTool control={control} />
          </div>
        )}
      </Form>
    );
  };
ManuallyTriggerFormValidation.storyName = '💠 Manually trigger form validation';
ManuallyTriggerFormValidation.parameters = {
  docs: {
    description: {
      story: `In this example, the form is automatically validated thanks to a \`useImperativeHandle\` hook.`,
    },
  },
};

export const ManuallyFocusField: ComponentStory<typeof Form> = () => {
  const validationSchema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  const formRef = React.useRef<UseFormReturn>();
  React.useEffect(() => {
    // Use useEffect hook to wait for the form to be rendered before triggering focus
    formRef?.current?.setFocus('codeEditorFieldName');
  });

  return (
    <Form
      id="formId"
      ref={formRef}
      schema={validationSchema}
      onSubmit={action('onSubmit')}
    >
      {({ control }) => (
        <div className="space-y-2">
          <h1 className="text-xl font-semibold mb-xs">Manually focus field</h1>
          <CodeEditorField
            name="codeEditorFieldName"
            label="The code editor field label"
          />
          <Button type="submit" mode="primary">
            Submit
          </Button>
          {/* Debug form state */}
          <DevTool control={control} />
        </div>
      )}
    </Form>
  );
};
ManuallyFocusField.storyName = '💠 Manually focus a field';
ManuallyFocusField.parameters = {
  docs: {
    description: {
      story: `In this example, the form \`codeEditorFieldName\` field is automatically focused thanks to a \`useImperativeHandle\` hook.`,
    },
  },
};

export const AllInputs: ComponentStory<typeof Form> = () => {
  const validationSchema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
    textareaName: z.string().min(1, { message: 'Mandatory field' }),
    selectName: z.string().min(1, { message: 'Mandatory field' }),
    checkboxNames: z
      // When nothing is selected, the value is a false boolean
      .union([z.string().array(), z.boolean()])
      .refine(
        value => Array.isArray(value) && value.length > 0,
        'Choose at least one option'
      ),
    radioName: z
      // When nothing is selected, the value is null
      .union([z.string(), z.null()])
      .refine(
        value => typeof value === 'string' && value.length > 0,
        'Choose one option'
      ),
    codeEditorFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  const formRef = React.useRef<UseFormReturn>();
  React.useEffect(() => {
    // Use useEffect hook to wait for the form to be rendered before triggering validation
    formRef?.current?.trigger();
  }, []);

  return (
    <Form
      id="formId"
      ref={formRef}
      options={{
        mode: 'onSubmit',
        reValidateMode: 'onChange',
      }}
      schema={validationSchema}
      onSubmit={action('onSubmit')}
    >
      {({ control, reset }) => (
        <div className="space-y-2">
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
          <Checkbox
            name="checkboxNames"
            label="The checkbox label *"
            description="The checkbox description"
            tooltip="The checkbox tooltip"
            options={[
              { value: 'checkboxValue0', label: 'Checkbox value 0' },
              {
                value: 'checkboxValue1',
                label: 'Checkbox value 1',
                disabled: true,
              },
              { value: 'checkboxValue2', label: 'Checkbox value 2' },
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
          <div className="flex gap-4">
            <Button type="button" onClick={() => reset({})}>
              Reset
            </Button>
            <Button type="submit" mode="primary">
              Submit
            </Button>
          </div>
          <DevTool control={control} />
        </div>
      )}
    </Form>
  );
};
AllInputs.storyName = '💠 Demo with all inputs and form reset';
AllInputs.parameters = {
  docs: {
    description: {
      story: `Validation schema with all inputs mandatory.`,
    },
  },
};

export const AllInputsHorizontal: ComponentStory<typeof Form> = () => {
  const validationSchema = z.object({
    inputFieldName: z.string().min(1, { message: 'Mandatory field' }),
    textareaName: z.string().min(1, { message: 'Mandatory field' }),
    selectName: z.string().min(1, { message: 'Mandatory field' }),
    checkboxNames: z
      // When nothing is selected, the value is a false boolean
      .union([z.string().array(), z.boolean()])
      .refine(
        value => Array.isArray(value) && value.length > 0,
        'Choose at least one option'
      ),
    radioName: z
      // When nothing is selected, the value is null
      .union([z.string(), z.null()])
      .refine(
        value => typeof value === 'string' && value.length > 0,
        'Choose one option'
      ),
    codeEditorFieldName: z.string().min(1, { message: 'Mandatory field' }),
  });

  const formRef = React.useRef<UseFormReturn>();
  React.useEffect(() => {
    // Use useEffect hook to wait for the form to be rendered before triggering validation
    formRef?.current?.trigger();
  });

  return (
    <Form
      id="formId"
      ref={formRef}
      options={{
        mode: 'onSubmit',
        reValidateMode: 'onChange',
      }}
      schema={validationSchema}
      onSubmit={action('onSubmit')}
    >
      {({ control, reset }) => (
        <div className="space-y-2">
          <h1 className="text-xl font-semibold mb-xs">Form title</h1>
          <InputField
            name="inputFieldName"
            label="The input field label"
            description="The input field description"
            tooltip="The input field tooltip"
            placeholder="Input field placeholder"
            horizontal
          />
          <Textarea
            name="textareaName"
            label="The textarea label"
            description="The textarea description"
            tooltip="The textarea tooltip"
            placeholder="Textarea field placeholder"
            horizontal
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
            horizontal
          />
          <Checkbox
            name="checkboxNames"
            label="The checkbox label *"
            description="The checkbox description"
            tooltip="The checkbox tooltip"
            options={[
              { value: 'checkboxValue0', label: 'Checkbox value 0' },
              {
                value: 'checkboxValue1',
                label: 'Checkbox value 1',
                disabled: true,
              },
              { value: 'checkboxValue2', label: 'Checkbox value 2' },
            ]}
            orientation="vertical"
            horizontal
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
            orientation="vertical"
            horizontal
          />
          <CodeEditorField
            name="codeEditorFieldName"
            label="The code editor label *"
            description="The code editor description"
            tooltip="The code editor tooltip"
            horizontal
          />
          <div className="flex gap-4">
            <Button type="button" onClick={() => reset({})}>
              Reset
            </Button>
            <Button type="submit" mode="primary">
              Submit
            </Button>
          </div>
          <DevTool control={control} />
        </div>
      )}
    </Form>
  );
};
AllInputsHorizontal.storyName = ' 💠 Demo with horizontal fields';
