import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { ConnectMssqlWidget } from './ConnectMssqlWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../mocks/handlers.mock';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

export default {
  component: ConnectMssqlWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof ConnectMssqlWidget>;

export const CreateConnection: StoryFn<typeof ConnectMssqlWidget> = () => {
  return (
    <div className="flex justify-center">
      <div className="w-1/2">
        <ConnectMssqlWidget />
      </div>
    </div>
  );
};

export const MSSQLCreateConnection: StoryObj<typeof ConnectMssqlWidget> = {
  render: () => {
    return (
      <div className="flex justify-center">
        <div className="w-1/2">
          <ConnectMssqlWidget />
        </div>
      </div>
    );
  },

  name: '🧪 MSSQL Interaction test (add database)',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // verify if the right title is displayed. It should contain the word `postgres`.
    await expect(
      await canvas.findByText('Connect MSSQL Database')
    ).toBeInTheDocument();

    // verify if all the fields are present (in oss mode)

    await expect(
      await canvas.findByLabelText('Database name')
    ).toBeInTheDocument();

    // There should be exactly 3 supported database connection options
    const radioOptions = await canvas.findAllByLabelText(
      'Connect Database via'
    );
    await expect(radioOptions.length).toBe(2);

    const databaseUrlOption = await canvas.findByTestId(
      'configuration.connectionInfo.connectionString.connectionType-databaseUrl'
    );
    await expect(databaseUrlOption).toBeInTheDocument();
    await userEvent.click(databaseUrlOption);
    await expect(databaseUrlOption).toBeChecked();

    // Expect the first option to have the following input fields
    await expect(
      await canvas.findByPlaceholderText(
        'Driver={ODBC Driver 18 for SQL Server};Server=serveraddress;Database=dbname;Uid=username;Pwd=password'
      )
    ).toBeInTheDocument();

    // click on the environment variable option and verify if the correct fields are shown
    const environmentVariableOption = await canvas.findByTestId(
      'configuration.connectionInfo.connectionString.connectionType-envVar'
    );
    await userEvent.click(environmentVariableOption);
    await expect(
      await canvas.findByPlaceholderText('HASURA_GRAPHQL_DB_URL_FROM_ENV')
    ).toBeInTheDocument();

    // Find and click on advanced settings
    await userEvent.click(await canvas.findByText('Advanced Settings'));
    await expect(
      await canvas.findByText('Total Max Connections')
    ).toBeInTheDocument();
    await expect(await canvas.findByText('Idle Timeout')).toBeInTheDocument();
  },
};

export const MSSQLEditConnection: StoryObj<typeof ConnectMssqlWidget> = {
  render: () => {
    return (
      <div className="flex justify-center">
        <div className="w-1/2">
          <ConnectMssqlWidget dataSourceName="mssql1" />
        </div>
      </div>
    );
  },

  name: '🧪 MSSQL Edit Databaase Interaction test',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // verify if the right title is displayed. It should contain the word `postgres`.
    await expect(
      await canvas.findByText('Edit MSSQL Connection')
    ).toBeInTheDocument();

    // verify if all the fields are present (in oss mode)

    await waitFor(
      async () => {
        await expect(await canvas.findByLabelText('Database name')).toHaveValue(
          'mssql1'
        );
      },
      { timeout: 5000 }
    );

    const radioOptions = await canvas.findAllByLabelText(
      'Connect Database via'
    );
    await expect(radioOptions.length).toBe(2);
    const databaseUrlOption = await canvas.findByTestId(
      'configuration.connectionInfo.connectionString.connectionType-databaseUrl'
    );
    await expect(databaseUrlOption).toBeChecked();
    await expect(
      await canvas.findByTestId(
        'configuration.connectionInfo.connectionString.url'
      )
    ).toHaveValue(
      'DRIVER={ODBC Driver 17 for SQL Server};SERVER=host.docker.internal;DATABASE=bikes;Uid=SA;Pwd=reallyStrongPwd123'
    );

    // Find and click on advanced settings
    await userEvent.click(await canvas.findByText('Advanced Settings'));
    await expect(
      await canvas.findByTestId(
        'configuration.connectionInfo.poolSettings.totalMaxConnections'
      )
    ).toHaveValue(50);
    await expect(
      await canvas.findByTestId(
        'configuration.connectionInfo.poolSettings.idleTimeout'
      )
    ).toHaveValue(180);

    // find and click on graphql customization settings
    await userEvent.click(await canvas.findByText('GraphQL Customization'));
    await expect(
      await canvas.findByTestId('customization.rootFields.namespace')
    ).toHaveValue('some_field_name');
    await expect(
      await canvas.findByTestId('customization.rootFields.prefix')
    ).toHaveValue('some_field_name_prefix');
    await expect(
      await canvas.findByTestId('customization.rootFields.suffix')
    ).toHaveValue('some_field_name_suffix');
    await expect(
      await canvas.findByTestId('customization.typeNames.prefix')
    ).toHaveValue('some_type_name_prefix');
    await expect(
      await canvas.findByTestId('customization.typeNames.suffix')
    ).toHaveValue('some_type_name_suffix');
  },
};
