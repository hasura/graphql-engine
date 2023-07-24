import { expect } from '@storybook/jest';
import { StoryObj, Meta } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import React from 'react';
import {
  CustomFieldNamesForm,
  CustomFieldNamesFormProps,
} from './CustomFieldNamesForm';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

export default {
  component: CustomFieldNamesForm,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof CustomFieldNamesForm>;

export const Primary: StoryObj<CustomFieldNamesFormProps> = {
  render: args => (
    <div className="w-[600px] h-auto overflow-auto border pt-4 mb-4 bg-white">
      <CustomFieldNamesForm {...args} />
    </div>
  ),

  args: {
    initialTableName: 'Customer',
  },

  play: async ({ canvasElement }) => {
    const c = within(canvasElement);

    const customTableInput = c.getByLabelText('Custom Table Name');
    const queryCollapse = c
      .getByText('Query and Subscription')
      .closest('button') as HTMLElement;
    const mutationCollapse = c
      .getByText('Mutation')
      .closest('button') as HTMLElement;
    const customTableClearButton = () =>
      customTableInput.parentElement?.querySelector('button') as HTMLElement;

    // testing entering value, replacing space with underscore, and clearing
    await userEvent.type(customTableInput, 'hello world');

    await expect(customTableInput).toHaveValue('hello_world');

    await userEvent.click(customTableClearButton());

    await expect(customTableInput).toHaveValue('');

    // test collapse
    await userEvent.click(queryCollapse);

    await expect(c.getByText('select_by_pk')).toBeVisible();

    await userEvent.click(queryCollapse);

    await expect(c.queryByText('select_by_pk')).not.toBeInTheDocument();

    // test mutation collapse
    await userEvent.click(mutationCollapse);

    await expect(c.getByText('delete_by_pk')).toBeVisible();

    await userEvent.click(mutationCollapse);

    await expect(c.queryByText('delete_by_pk')).not.toBeInTheDocument();

    // test clear all fields
    const clearAllFieldsButton = c.getByRole('button', {
      name: 'Clear All Fields',
    });
    await expect(clearAllFieldsButton).toBeDisabled();

    // open collapsible areas
    await userEvent.click(mutationCollapse);
    await userEvent.click(queryCollapse);

    const form = canvasElement.querySelector('form');

    const inputs = form?.querySelectorAll('input[type="text"]');

    inputs?.forEach(async input => {
      await userEvent.type(input, 'foo');
    });

    inputs?.forEach(async input => {
      await expect(input).toHaveValue('foo');
    });

    await userEvent.click(clearAllFieldsButton);

    inputs?.forEach(async input => {
      await expect(input).toHaveValue('');
    });

    // close collapsed form sections
    await userEvent.click(mutationCollapse);
    await userEvent.click(queryCollapse);
  },
};
