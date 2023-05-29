import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { StoryObj, StoryFn, Meta } from '@storybook/react';
import React from 'react';
import { screen } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { RenderOpenApi3Form } from './common/RenderOpenApi3Form';

export default {
  title: 'Components/OpenApi3Form ⚛️/ObjectInput',
  parameters: {
    docs: {
      description: {
        component: `This component demonstrates how to use a complex object input via the OpenApi3Form component`,
      },
      source: { type: 'code' },
    },
  },
  component: RenderOpenApi3Form,
  decorators: [
    ReactQueryDecorator(),
    Story => <div className="p-4 w-full">{Story()}</div>,
  ],
} as Meta<typeof RenderOpenApi3Form>;

export const ObjectInputWithFields: StoryFn<typeof RenderOpenApi3Form> = () => {
  return (
    <RenderOpenApi3Form
      getSchema={() => [
        {
          nullable: false,
          properties: {
            jdbc_url: {
              nullable: false,
              type: 'string',
            },
            primary_keys: {
              items: {
                nullable: false,
                properties: {
                  columns: {
                    items: {
                      nullable: false,
                      type: 'string',
                    },
                    nullable: false,
                    type: 'array',
                  },
                  table: {
                    items: {
                      nullable: false,
                      type: 'string',
                    },
                    nullable: false,
                    type: 'array',
                  },
                },
                type: 'object',
              },
              nullable: true,
              type: 'array',
            },
            schema: {
              nullable: true,
              type: 'string',
            },
            tables: {
              items: {
                nullable: false,
                type: 'string',
              },
              nullable: true,
              type: 'array',
            },
          },
          type: 'object',
        },
        {},
      ]}
      defaultValues={{
        ObjectProperty: {
          jdbc_url: 'asdasd',
          primary_keys: [
            {
              columns: ['test'],
              table: ['test'],
            },
            {
              columns: ['test'],
              table: ['test'],
            },
          ],
          schema: 'asdasd',
          tables: ['asd', ' value'],
        },
      }}
      name="ObjectProperty"
    />
  );
};

export const ObjectInputArrayInput: StoryFn<typeof RenderOpenApi3Form> = () => {
  return (
    <RenderOpenApi3Form
      getSchema={() => [
        {
          title: 'Available Connections',
          type: 'array',
          nullable: true,
          items: {
            $ref: '#/otherSchemas/ConnectionParams',
          },
        },
        {
          ConnectionParams: {
            title: 'Connection Parameters',
            type: 'object',
            nullable: true,
            properties: {
              username: {
                title: 'Username',
                type: 'string',
                nullable: true,
              },
              database: {
                title: 'Database',
                type: 'string',
                nullable: true,
              },
              host: {
                title: 'Host',
                type: 'string',
                // nullable: true,
              },
              port: {
                title: 'Port',
                type: 'number',
                nullable: true,
              },
              socket: {
                title: 'Socket',
                type: 'number',
                nullable: true,
              },
              uuid: {
                title: 'UUID',
                type: 'number',
                nullable: true,
              },
              disable_telemetry: {
                title: 'Telemetry',
                type: 'boolean',
                nullable: true,
              },
              disable_auto_update: {
                title: 'Auto updates',
                type: 'boolean',
                nullable: true,
              },
              enable_log: {
                title: 'Logging',
                type: 'boolean',
                nullable: true,
              },
            },
          },
        },
      ]}
      defaultValues={{}}
      name="ObjectProperty"
    />
  );
};

export const Test: StoryObj<typeof RenderOpenApi3Form> = {
  render: () => (
    <RenderOpenApi3Form
      getSchema={() => [
        {
          title: 'Connection Parameters',
          type: 'object',
          nullable: false,
          properties: {
            username: {
              title: 'Username',
              type: 'string',
              nullable: false,
            },
            port: {
              title: 'Port',
              type: 'number',
              nullable: false,
            },
            enable_log: {
              title: 'Enable Log',
              type: 'boolean',
            },
          },
        },
        {},
      ]}
      defaultValues={{}}
      name="ObjectProperty"
      rawOutput
    />
  ),

  name: '🧪 Testing - input interaction',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await waitFor(async () => {
      await userEvent.type(
        canvas.getByTestId('ObjectProperty.username'),
        'foobar'
      );
      await userEvent.type(canvas.getByTestId('ObjectProperty.port'), '1234');

      await userEvent.click(canvas.getByTestId('ObjectProperty.enable_log'));
    });

    await waitFor(async () => {
      await userEvent.click(canvas.getByTestId('submit-form-btn'));
    });

    await waitFor(async () => {
      await expect(screen.getByTestId('output').textContent).toBe(
        '{"ObjectProperty":{"username":"foobar","port":1234,"enable_log":true}}'
      );
    });
  },
};
