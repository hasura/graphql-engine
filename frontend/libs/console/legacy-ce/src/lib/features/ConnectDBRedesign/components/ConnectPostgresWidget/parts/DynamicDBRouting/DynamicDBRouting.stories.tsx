import { expect } from '@storybook/jest';
import { StoryObj, Meta } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { handlers } from '../../../../../../mocks/metadata.mock';
import { ReactQueryDecorator } from '../../../../../../storybook/decorators/react-query';
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
      () => {
        expect(canvas.getByLabelText('Database Tenancy')).toBeInTheDocument();
      },
      { timeout: 2000 }
    );

    // click on Database Tenancy
    const radioTenancy = canvas.getByLabelText('Database Tenancy');
    userEvent.click(radioTenancy);

    // click on "Add Connection"
    const buttonAddConnection = canvas.getByText('Add Connection');
    userEvent.click(buttonAddConnection);

    // write "test" in the input text with testid "name"
    const inputName = canvas.getByTestId('name');
    userEvent.type(inputName, 'test');

    // write "test" in the input text with testid "configuration.connectionInfo.databaseUrl.url"
    const inputDatabaseUrl = canvas.getByTestId(
      'configuration.connectionInfo.databaseUrl.url'
    );
    userEvent.type(inputDatabaseUrl, 'test');

    // click on submit
    const buttonSubmit = canvas.getAllByText('Add Connection')[1];
    userEvent.click(buttonSubmit);
  },
};
