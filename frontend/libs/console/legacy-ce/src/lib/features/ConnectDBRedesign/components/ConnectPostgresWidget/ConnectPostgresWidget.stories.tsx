import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { ConnectPostgresWidget } from './ConnectPostgresWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../mocks/handlers.mock';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';

export default {
  component: ConnectPostgresWidget,
  decorators: [
    ReactQueryDecorator(),
    ConsoleTypeDecorator({ consoleType: 'pro' }),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof ConnectPostgresWidget>;

export const PostgresCreateConnection: StoryFn<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="flex justify-center">
      <div className="w-1/2">
        <ConnectPostgresWidget />
      </div>
    </div>
  );
};

export const Test: StoryObj<typeof ConnectPostgresWidget> = {
  render: () => {
    return (
      <div className="flex justify-center">
        <div className="w-1/2">
          <ConnectPostgresWidget />
        </div>
      </div>
    );
  },

  name: '🧪 Postgres Interaction test (add database)',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // verify if the right title is displayed. It should contain the word `postgres`.
    await expect(
      await canvas.findByText('Connect Postgres Database')
    ).toBeInTheDocument();

    // verify if all the fields are present (in oss mode)

    await expect(
      await canvas.findByLabelText('Database name')
    ).toBeInTheDocument();

    // There should be exactly 3 supported database connection options
    const radioOptions = await canvas.findAllByLabelText(
      'Connect Database via'
    );
    await expect(radioOptions.length).toBe(4);

    const databaseUrlOption = await canvas.findByTestId(
      'configuration.connectionInfo.databaseUrl.connectionType-databaseUrl'
    );
    await expect(databaseUrlOption).toBeInTheDocument();
    await userEvent.click(databaseUrlOption);

    // Expect the first option to have the following input fields
    await expect(
      await canvas.findByPlaceholderText(
        'postgresql://username:password@hostname:port/postgres'
      )
    ).toBeInTheDocument();

    // click on the environment variable option and verify if the correct fields are shown
    const environmentVariableOption = await canvas.findByTestId(
      'configuration.connectionInfo.databaseUrl.connectionType-envVar'
    );
    await userEvent.click(environmentVariableOption);
    await expect(
      await canvas.findByPlaceholderText('HASURA_GRAPHQL_DB_URL_FROM_ENV')
    ).toBeInTheDocument();

    // click on the connection parameters option and verify if the correct fields are shown
    const connectionParamsOption = await canvas.findByTestId(
      'configuration.connectionInfo.databaseUrl.connectionType-connectionParams'
    );
    await userEvent.click(connectionParamsOption);
    await expect(
      await canvas.findByPlaceholderText('postgres_user')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByPlaceholderText('password')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByPlaceholderText('postgres')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByPlaceholderText('localhost')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByPlaceholderText('5432')
    ).toBeInTheDocument();

    // Find and click on advanced settings
    await userEvent.click(await canvas.findByText('Advanced Settings'));
    await expect(
      await canvas.findByText('Total Max Connections')
    ).toBeInTheDocument();
    await expect(await canvas.findByText('Idle Timeout')).toBeInTheDocument();
    await expect(await canvas.findByText('Retries')).toBeInTheDocument();
    await expect(await canvas.findByText('Pool Timeout')).toBeInTheDocument();
    await expect(
      await canvas.findByText('Connection Lifetime')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByText('Isolation Level')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByText('Use Prepared Statements')
    ).toBeInTheDocument();
    await expect(
      await canvas.findByText('Extension Schema')
    ).toBeInTheDocument();
  },
};

export const PostgresEditConnection: StoryObj<typeof ConnectPostgresWidget> = {
  render: () => {
    return (
      <div className="max-w-3xl">
        <ConnectPostgresWidget dataSourceName="chinook" />
      </div>
    );
  },

  name: '🧪 Postgres Edit Databaase Interaction test',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // verify if the right title is displayed. It should contain the word `postgres`.
    await expect(
      await canvas.findByText('Edit Postgres Connection')
    ).toBeInTheDocument();

    // verify if all the fields are present (in oss mode)

    await waitFor(
      async () => {
        await expect(await canvas.findByLabelText('Database name')).toHaveValue(
          'chinook'
        );
      },
      { timeout: 5000 }
    );

    const radioOptions = await canvas.findAllByLabelText(
      'Connect Database via'
    );
    await expect(radioOptions.length).toBe(4);
    const databaseUrlOption = await canvas.findByTestId(
      'configuration.connectionInfo.databaseUrl.connectionType-databaseUrl'
    );
    await expect(databaseUrlOption).toBeChecked();
    await expect(
      await canvas.findByTestId('configuration.connectionInfo.databaseUrl.url')
    ).toHaveValue('postgres://postgres:test@host.docker.internal:6001/chinook');

    // Find and click on advanced settings
    await userEvent.click(await canvas.findByText('Advanced Settings'));
    await expect(
      await canvas.findByTestId(
        'configuration.connectionInfo.poolSettings.totalMaxConnections'
      )
    ).toHaveValue(500);
    await expect(
      await canvas.findByTestId(
        'configuration.connectionInfo.poolSettings.idleTimeout'
      )
    ).toHaveValue(200);
    await expect(
      await canvas.findByTestId(
        'configuration.connectionInfo.poolSettings.retries'
      )
    ).toHaveValue(400);
    await expect(
      await canvas.findByTestId(
        'configuration.connectionInfo.poolSettings.poolTimeout'
      )
    ).toHaveValue(300);
    await expect(
      await canvas.findByTestId(
        'configuration.connectionInfo.poolSettings.connectionLifetime'
      )
    ).toHaveValue(100);

    // find and click on graphql customization settings
    await userEvent.click(await canvas.findByText('GraphQL Customization'));
    await expect(
      await canvas.findByTestId('customization.rootFields.namespace')
    ).toHaveValue('namespace_');
    await expect(
      await canvas.findByTestId('customization.rootFields.prefix')
    ).toHaveValue('prefix_');
    await expect(
      await canvas.findByTestId('customization.rootFields.suffix')
    ).toHaveValue('_suffix');
    await expect(
      await canvas.findByTestId('customization.typeNames.prefix')
    ).toHaveValue('prefix_');
    await expect(
      await canvas.findByTestId('customization.typeNames.suffix')
    ).toHaveValue('_suffix');
  },
};

export const AlloyDBCreateConnection: StoryFn<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget overrideDisplayName="AlloyDB" />
    </div>
  );
};

export const CitusCreateConnection: StoryFn<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget
        overrideDisplayName="Citus"
        overrideDriver="citus"
      />
    </div>
  );
};

export const CitusEditConnection: StoryFn<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget
        overrideDisplayName="Citus"
        overrideDriver="citus"
        dataSourceName="citus_test"
      />
    </div>
  );
};

export const CockroachCreateConnection: StoryFn<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget
        overrideDisplayName="CockroachDB"
        overrideDriver="cockroach"
      />
    </div>
  );
};
