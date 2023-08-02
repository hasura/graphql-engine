import { StoryObj, Meta } from '@storybook/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { handlers } from './mocks/handlers';
import {
  FunctionGraphQLCustomization,
  FunctionGraphQLCustomizationProps,
} from './FunctionGraphQLCustomization';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      cacheTime: 0,
    },
  },
});

export default {
  component: FunctionGraphQLCustomization,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
  decorators: [
    (Story: React.FC) => (
      <div className="max-w-xl">
        <QueryClientProvider client={queryClient}>
          <Story />
        </QueryClientProvider>
      </div>
    ),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof FunctionGraphQLCustomization>;

export const Primary: StoryObj<FunctionGraphQLCustomizationProps> = {
  args: {
    dataSourceName: 'aPostgres',
    driver: 'postgres',
    qualifiedFunction: {
      name: 'search_album',
      schema: 'public',
    },
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByText('Custom Field Names')).toBeVisible();
    await expect(canvas.getByText('(Learn More)')).toBeVisible();
    await expect(canvas.getByText('Add Custom Field Names')).toBeVisible();

    await userEvent.click(canvas.getByText('Add Custom Field Names'));

    await waitFor(async () => {
      await expect(await canvas.getByText('search_album')).toBeVisible();
    });

    await expect(
      canvas.getByText(
        'GraphQL fields are limited to letters, numbers, and underscores.'
      )
    ).toBeVisible();
    await expect(
      canvas.getByText('Any spaces are converted to underscores.')
    ).toBeVisible();

    await expect(canvas.getAllByText('Clear All Fields')).toHaveLength(1);

    await expect(canvas.getByLabelText('Custom Function Name')).toBeVisible();
    await expect(
      canvas.getByLabelText('Function name root field')
    ).toBeVisible();
    await expect(
      canvas.getByLabelText('Function aggregate root field')
    ).toBeVisible();

    const customNameInput = canvas.getByPlaceholderText('custom_name');
    await expect(customNameInput).toBeVisible();
    await userEvent.type(customNameInput, 'my_custom_name');
    await expect(customNameInput).toHaveValue('my_custom_name');

    const functionNameInput = canvas.getByPlaceholderText('function_name');
    await expect(functionNameInput).toBeVisible();
    await userEvent.type(functionNameInput, 'my_function_name');
    await expect(functionNameInput).toHaveValue('my_function_name');

    const functionAggregateInput =
      canvas.getByPlaceholderText('function_aggregate');
    await expect(functionAggregateInput).toBeVisible();
    await userEvent.type(functionAggregateInput, 'my_function_aggregate');
    await expect(functionAggregateInput).toHaveValue('my_function_aggregate');

    await expect(canvas.getByText('Cancel')).toBeVisible();
    await expect(canvas.getByText('Save')).toBeVisible();

    await userEvent.click(canvas.getByText('Clear All Fields'));
    await expect(customNameInput).toHaveValue('');
    await expect(functionNameInput).toHaveValue('');
    await expect(functionAggregateInput).toHaveValue('');

    await userEvent.click(canvas.getByText('Cancel'));

    await expect(
      canvas.getByText('No custom fields are currently set.')
    ).toBeVisible();
  },
};
