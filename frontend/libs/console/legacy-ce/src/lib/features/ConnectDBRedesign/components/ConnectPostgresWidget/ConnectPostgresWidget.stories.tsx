import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ConnectPostgresWidget } from './ConnectPostgresWidget';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../mocks/handlers.mock';
import { screen, userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

export default {
  component: ConnectPostgresWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ConnectPostgresWidget>;

export const PostgresCreateConnection: ComponentStory<
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

export const Test: ComponentStory<typeof ConnectPostgresWidget> = () => {
  return (
    <div className="flex justify-center">
      <div className="w-1/2">
        <ConnectPostgresWidget />
      </div>
    </div>
  );
};

Test.storyName = '🧪 Postgres Interaction test (add database)';

Test.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // verify if the right title is displayed. It should contain the word `postgres`.
  expect(
    await canvas.findByText('Connect Postgres Database')
  ).toBeInTheDocument();

  // verify if all the fields are present (in oss mode)

  expect(await canvas.findByLabelText('Database name')).toBeInTheDocument();

  // There should be exactly 3 supported database connection options
  const radioOptions = await canvas.findAllByLabelText('Connect Database via');
  expect(radioOptions.length).toBe(3);

  const databaseUrlOption = await canvas.findByTestId(
    'configuration.connectionInfo.databaseUrl.connectionType-databaseUrl'
  );
  expect(databaseUrlOption).toBeInTheDocument();
  expect(databaseUrlOption).toBeChecked();

  // Expect the first option to have the following input fields
  expect(
    await canvas.findByPlaceholderText(
      'postgresql://username:password@hostname:port/postgres'
    )
  ).toBeInTheDocument();

  // click on the environment variable option and verify if the correct fields are shown
  const environmentVariableOption = await canvas.findByTestId(
    'configuration.connectionInfo.databaseUrl.connectionType-envVar'
  );
  userEvent.click(environmentVariableOption);
  expect(
    await canvas.findByPlaceholderText('HASURA_GRAPHQL_DB_URL_FROM_ENV')
  ).toBeInTheDocument();

  // click on the connection parameters option and verify if the correct fields are shown
  const connectionParamsOption = await canvas.findByTestId(
    'configuration.connectionInfo.databaseUrl.connectionType-connectionParams'
  );
  userEvent.click(connectionParamsOption);
  expect(
    await canvas.findByPlaceholderText('postgres_user')
  ).toBeInTheDocument();
  expect(await canvas.findByPlaceholderText('password')).toBeInTheDocument();
  expect(await canvas.findByPlaceholderText('postgres')).toBeInTheDocument();
  expect(await canvas.findByPlaceholderText('localhost')).toBeInTheDocument();
  expect(await canvas.findByPlaceholderText('5432')).toBeInTheDocument();

  // Find and click on advanced settings
  userEvent.click(await canvas.findByText('Advanced Settings'));
  expect(await canvas.findByText('Total Max Connections')).toBeInTheDocument();
  expect(await canvas.findByText('Idle Timeout')).toBeInTheDocument();
  expect(await canvas.findByText('Retries')).toBeInTheDocument();
  expect(await canvas.findByText('Pool Timeout')).toBeInTheDocument();
  expect(await canvas.findByText('Connection Lifetime')).toBeInTheDocument();
  expect(await canvas.findByText('Isolation Level')).toBeInTheDocument();
  expect(
    await canvas.findByText('Use Prepared Statements')
  ).toBeInTheDocument();
  expect(await canvas.findByText('Extension Schema')).toBeInTheDocument();
};

export const PostgresEditConnection: ComponentStory<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget dataSourceName="chinook" />
    </div>
  );
};

PostgresEditConnection.storyName =
  '🧪 Postgres Edit Databaase Interaction test';
PostgresEditConnection.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // verify if the right title is displayed. It should contain the word `postgres`.
  expect(
    await canvas.findByText('Edit Postgres Connection')
  ).toBeInTheDocument();

  // verify if all the fields are present (in oss mode)

  await waitFor(
    async () => {
      expect(await canvas.findByLabelText('Database name')).toHaveValue(
        'chinook'
      );
    },
    { timeout: 5000 }
  );

  const radioOptions = await canvas.findAllByLabelText('Connect Database via');
  expect(radioOptions.length).toBe(3);
  const databaseUrlOption = await canvas.findByTestId(
    'configuration.connectionInfo.databaseUrl.connectionType-databaseUrl'
  );
  expect(databaseUrlOption).toBeChecked();
  expect(
    await canvas.findByTestId('configuration.connectionInfo.databaseUrl.url')
  ).toHaveValue('postgres://postgres:test@host.docker.internal:6001/chinook');

  // Find and click on advanced settings
  userEvent.click(await canvas.findByText('Advanced Settings'));
  expect(
    await canvas.findByTestId(
      'configuration.connectionInfo.poolSettings.totalMaxConnections'
    )
  ).toHaveValue(500);
  expect(
    await canvas.findByTestId(
      'configuration.connectionInfo.poolSettings.idleTimeout'
    )
  ).toHaveValue(200);
  expect(
    await canvas.findByTestId(
      'configuration.connectionInfo.poolSettings.retries'
    )
  ).toHaveValue(400);
  expect(
    await canvas.findByTestId(
      'configuration.connectionInfo.poolSettings.poolTimeout'
    )
  ).toHaveValue(300);
  expect(
    await canvas.findByTestId(
      'configuration.connectionInfo.poolSettings.connectionLifetime'
    )
  ).toHaveValue(100);

  // find and click on graphql customization settings
  userEvent.click(await canvas.findByText('GraphQL Customization'));
  expect(
    await canvas.findByTestId('customization.rootFields.namespace')
  ).toHaveValue('namespace_');
  expect(
    await canvas.findByTestId('customization.rootFields.prefix')
  ).toHaveValue('prefix_');
  expect(
    await canvas.findByTestId('customization.rootFields.suffix')
  ).toHaveValue('_suffix');
  expect(
    await canvas.findByTestId('customization.typeNames.prefix')
  ).toHaveValue('prefix_');
  expect(
    await canvas.findByTestId('customization.typeNames.suffix')
  ).toHaveValue('_suffix');
};

export const AlloyDBCreateConnection: ComponentStory<
  typeof ConnectPostgresWidget
> = () => {
  return (
    <div className="max-w-3xl">
      <ConnectPostgresWidget overrideDisplayName="AlloyDB" />
    </div>
  );
};

export const CitusCreateConnection: ComponentStory<
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

export const CitusEditConnection: ComponentStory<
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

export const CockroachCreateConnection: ComponentStory<
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
