import { expect, userEvent, waitFor, within } from 'storybook/test';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { handlers, ReactQueryDecorator } from '@hasura/shared/testing';
import { DynamicDBRouting } from './DynamicDBRouting';

export default {
  component: DynamicDBRouting,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta<typeof DynamicDBRouting>;

export const Default: StoryObj<typeof DynamicDBRouting> = {
  render: () => <DynamicDBRouting sourceName="default" />,

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    await waitFor(
      async () => {
        await expect(
          canvas.getByLabelText('Database Tenancy'),
        ).toBeInTheDocument();
      },
      { timeout: 2000 },
    );

    // click on Database Tenancy
    const radioTenancy = canvas.getByLabelText('Database Tenancy');
    await userEvent.click(radioTenancy);

    // click on "Add Connection"
    const buttonAddConnection = canvas.getByText('Add Connection');
    await userEvent.click(buttonAddConnection);

    // write "test" in the input text with testid "name"
    const inputName = canvas.getByTestId('name');
    await userEvent.type(inputName, 'test');

    // write "test" in the input text with testid "configuration.connectionInfo.databaseUrl.url"
    const inputDatabaseUrl = canvas.getByTestId(
      'configuration.connectionInfo.databaseUrl.url',
    );
    await userEvent.type(inputDatabaseUrl, 'test');

    // click on submit
    const buttonSubmit = canvas.getAllByText('Add Connection')[1];
    await userEvent.click(buttonSubmit);
  },
};
