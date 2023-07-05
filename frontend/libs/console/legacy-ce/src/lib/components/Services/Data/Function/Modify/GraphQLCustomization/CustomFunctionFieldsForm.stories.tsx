import { expect } from '@storybook/jest';
import { StoryObj, Meta } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import {
  CustomFunctionFieldsForm,
  CustomFunctionFieldsFormProps,
} from './CustomFunctionFieldsForm';

export default {
  component: CustomFunctionFieldsForm,
} as Meta<typeof CustomFunctionFieldsForm>;

export const Primary: StoryObj<CustomFunctionFieldsFormProps> = {
  render: args => (
    <div className="w-[600px] h-auto overflow-auto border pt-4 mb-4 bg-white">
      <CustomFunctionFieldsForm {...args} />
    </div>
  ),

  args: {},

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const clearAllFieldsButton = canvas.getByText('Clear All Fields');

    await expect(clearAllFieldsButton).toBeVisible();

    await expect(canvas.getByLabelText('Custom Function Name')).toBeVisible();
    await expect(canvas.getByPlaceholderText('custom_name')).toBeVisible();

    await expect(
      canvas.getByLabelText('Function name root field')
    ).toBeVisible();
    await expect(canvas.getByPlaceholderText('function_name')).toBeVisible();

    await expect(
      canvas.getByLabelText('Function aggregate root field')
    ).toBeVisible();
    await expect(
      canvas.getByPlaceholderText('function_aggregate')
    ).toBeVisible();

    await expect(canvas.getByText('Cancel')).toBeVisible();
    await expect(canvas.getByText('Save')).toBeVisible();

    const customNameInput = canvas.getByPlaceholderText('custom_name');
    await userEvent.type(customNameInput, 'my_custom_name');
    await expect(customNameInput).toHaveValue('my_custom_name');

    const functionNameInput = canvas.getByPlaceholderText('function_name');
    await userEvent.type(functionNameInput, 'my_function_name');
    await expect(functionNameInput).toHaveValue('my_function_name');

    const functionAggregateInput =
      canvas.getByPlaceholderText('function_aggregate');
    await userEvent.type(functionAggregateInput, 'my_function_aggregate');
    await expect(functionAggregateInput).toHaveValue('my_function_aggregate');

    await userEvent.click(clearAllFieldsButton);
    await expect(customNameInput).toHaveValue('');
    await expect(functionNameInput).toHaveValue('');
    await expect(functionAggregateInput).toHaveValue('');
  },
};

export const WithDefaultValues: StoryObj<CustomFunctionFieldsFormProps> = {
  render: args => (
    <div className="w-[600px] h-auto overflow-auto border pt-4 mb-4 bg-white">
      <CustomFunctionFieldsForm {...args} />
    </div>
  ),

  args: {
    defaultValues: {
      custom_name: 'a_custom_name',
      function: 'a_function',
      function_aggregate: 'a_function_aggregate',
    },
  },
};
