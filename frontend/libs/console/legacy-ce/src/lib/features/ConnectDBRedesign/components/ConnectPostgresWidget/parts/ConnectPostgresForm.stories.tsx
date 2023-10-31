import { SimpleForm } from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { Meta, StoryObj } from '@storybook/react';

import { ConnectPostgresForm } from './ConnectPostgresForm';
import { PostgresConnectionSchema, getDefaultValues, schema } from '../schema';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { useState } from 'react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

export default {
  component: ConnectPostgresForm,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof ConnectPostgresForm>;

export const TestPostgresForm: StoryObj<typeof ConnectPostgresForm> = {
  render: () => {
    const [formValues, setFormValues] = useState<PostgresConnectionSchema>();
    return (
      <SimpleForm
        onSubmit={data => setFormValues(data)}
        schema={schema}
        options={{
          defaultValues: getDefaultValues(),
        }}
      >
        <ConnectPostgresForm hiddenOptions={[]} />
        <Button type="submit" className="my-2">
          Submit
        </Button>
        <div data-testid="result">{JSON.stringify(formValues)}</div>
      </SimpleForm>
    );
  },
  name: '🧪 Add database with various configurations',
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const databaseUrlOption = await canvas.findByTestId(
      'configuration.connectionInfo.databaseUrl.connectionType-databaseUrl'
    );
    await expect(databaseUrlOption).toBeInTheDocument();
    await userEvent.click(databaseUrlOption);

    // verify if all the fields are present (in oss mode)

    await userEvent.type(
      await canvas.findByLabelText('Database name'),
      'chinook'
    );

    await userEvent.type(
      await canvas.findByPlaceholderText(
        'postgresql://username:password@hostname:port/postgres'
      ),
      'postgresql://myusername:password123@localhost:5432/chinook'
    );

    await userEvent.click(await canvas.findByText('Submit'));

    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('result')).toHaveTextContent(
          JSON.stringify({
            name: 'chinook',
            configuration: {
              connectionInfo: {
                databaseUrl: {
                  connectionType: 'databaseUrl',
                  url: 'postgresql://myusername:password123@localhost:5432/chinook',
                },
              },
            },
          })
        );
      },
      {
        timeout: 5000,
      }
    );

    await userEvent.click(
      await canvas.findByTestId(
        'configuration.connectionInfo.databaseUrl.connectionType-envVar'
      )
    );

    await userEvent.type(
      await canvas.findByPlaceholderText('HASURA_GRAPHQL_DB_URL_FROM_ENV'),
      'MY_SECRET_ENV_VAR'
    );

    await userEvent.click(await canvas.findByText('Submit'));

    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('result')).toHaveTextContent(
          JSON.stringify({
            name: 'chinook',
            configuration: {
              connectionInfo: {
                databaseUrl: {
                  connectionType: 'envVar',
                  envVar: 'MY_SECRET_ENV_VAR',
                },
              },
            },
          })
        );
      },
      {
        timeout: 5000,
      }
    );

    await userEvent.click(
      await canvas.findByTestId(
        'configuration.connectionInfo.databaseUrl.connectionType-connectionParams'
      )
    );

    await userEvent.type(
      await canvas.findByPlaceholderText('postgres_user'),
      'myusername'
    );
    await userEvent.type(
      await canvas.findByPlaceholderText('password'),
      'password123'
    );
    await userEvent.type(
      await canvas.findByPlaceholderText('postgres'),
      'chinook'
    );
    await userEvent.type(
      await canvas.findByPlaceholderText('localhost'),
      'localhost'
    );
    await userEvent.type(await canvas.findByPlaceholderText('5432'), '5432');

    await userEvent.click(await canvas.findByText('Submit'));

    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('result')).toHaveTextContent(
          JSON.stringify({
            name: 'chinook',
            configuration: {
              connectionInfo: {
                databaseUrl: {
                  connectionType: 'connectionParams',
                  username: 'myusername',
                  password: 'password123',
                  database: 'chinook',
                  host: 'localhost',
                  port: 5432,
                },
              },
            },
          })
        );
      },
      {
        timeout: 5000,
      }
    );

    await userEvent.click(await canvas.findByText('Advanced Settings'));
    await userEvent.type(await canvas.findByPlaceholderText('1000'), '100');
    await userEvent.type(await canvas.findByPlaceholderText('180'), '100');
    await userEvent.type(await canvas.findByPlaceholderText('1'), '100');
    await userEvent.type(await canvas.findByPlaceholderText('360'), '100');
    await userEvent.type(await canvas.findByPlaceholderText('600'), '100100');
    await userEvent.type(
      await canvas.findByPlaceholderText('public'),
      'public_schema'
    );

    await userEvent.click(await canvas.findByText('Submit'));

    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('result')).toHaveTextContent(
          JSON.stringify({
            name: 'chinook',
            configuration: {
              connectionInfo: {
                databaseUrl: {
                  connectionType: 'connectionParams',
                  username: 'myusername',
                  password: 'password123',
                  database: 'chinook',
                  host: 'localhost',
                  port: 5432,
                },
                poolSettings: {
                  totalMaxConnections: 100,
                  idleTimeout: 100,
                  retries: 100,
                  poolTimeout: 100,
                  connectionLifetime: 100100,
                },
                isolationLevel: 'read-committed',
              },
              extensionSchema: 'public_schema',
            },
          })
        );
      },
      {
        timeout: 5000,
      }
    );

    await userEvent.click(await canvas.findByText('GraphQL Customization'));
    await userEvent.type(
      await canvas.findByTestId('customization.rootFields.namespace'),
      'root_field_namespace'
    );
    await userEvent.type(
      await canvas.findByTestId('customization.rootFields.prefix'),
      'root_field_prefix'
    );
    await userEvent.type(
      await canvas.findByTestId('customization.rootFields.suffix'),
      'root_field_suffix'
    );
    await userEvent.type(
      await canvas.findByTestId('customization.typeNames.prefix'),
      'type_names_prefix'
    );
    await userEvent.type(
      await canvas.findByTestId('customization.typeNames.suffix'),
      'type_names_suffix'
    );

    await userEvent.click(await canvas.findByText('Submit'));

    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('result')).toHaveTextContent(
          JSON.stringify({
            name: 'chinook',
            configuration: {
              connectionInfo: {
                databaseUrl: {
                  connectionType: 'connectionParams',
                  username: 'myusername',
                  password: 'password123',
                  database: 'chinook',
                  host: 'localhost',
                  port: 5432,
                },
                poolSettings: {
                  totalMaxConnections: 100,
                  idleTimeout: 100,
                  retries: 100,
                  poolTimeout: 100,
                  connectionLifetime: 100100,
                },
                isolationLevel: 'read-committed',
              },
              extensionSchema: 'public_schema',
            },
            customization: {
              rootFields: {
                namespace: 'root_field_namespace',
                prefix: 'root_field_prefix',
                suffix: 'root_field_suffix',
              },
              typeNames: {
                prefix: 'type_names_prefix',
                suffix: 'type_names_suffix',
              },
            },
          })
        );
      },
      {
        timeout: 5000,
      }
    );
  },
};
