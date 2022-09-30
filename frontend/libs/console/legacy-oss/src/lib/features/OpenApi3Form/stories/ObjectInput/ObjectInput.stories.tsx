import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { screen } from '@testing-library/dom';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { RenderOpenApi3Form } from '../common/RenderOpenApi3Form';

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
} as ComponentMeta<typeof RenderOpenApi3Form>;

export const ObjectInputWithFields: ComponentStory<typeof RenderOpenApi3Form> =
  () => {
    return (
      <RenderOpenApi3Form
        getSchema={() => [
          {
            title: 'Connection Parameters',
            type: 'object',
            nullable: false,
            properties: {
              username: {
                title: 'Text Input (nullable explicitly set to false)',
                type: 'string',
                nullable: false,
              },
              database: {
                title: 'Text Input (nullable explicitly set to true)',
                type: 'string',
                nullable: true,
              },
              host: {
                title: 'Text Input',
                type: 'string',
              },
              port: {
                title: 'Text Input',
                type: 'number',
                nullable: false,
              },
              socket: {
                title: 'Text Input',
                type: 'number',
                nullable: true,
              },
              uuid: {
                title: 'Text Input',
                type: 'number',
              },
              disable_telemetry: {
                title: 'Text Input',
                type: 'boolean',
                nullable: false,
              },
              disable_auto_update: {
                title: 'Text Input',
                type: 'boolean',
                nullable: true,
              },
              enable_log: {
                title: 'Text Input',
                type: 'boolean',
              },
            },
          },
          {},
        ]}
        defaultValues={{}}
        name="ObjectProperty"
      />
    );
  };

export const ObjectInputArrayInput: ComponentStory<typeof RenderOpenApi3Form> =
  () => {
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

export const Test: ComponentStory<typeof RenderOpenApi3Form> = () => (
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
);

Test.storyName = '🧪 Testing - input interaction';

Test.play = async ({ canvasElement }) => {
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
};
