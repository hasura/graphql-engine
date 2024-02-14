import React from 'react';
import { RouteComponentProps } from 'react-router';
import { StoryObj, Meta } from '@storybook/react';
import { rest, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from 'react-query';

import { eeLicenseInfo } from '../../../features/EETrial/mocks/http';
import Sidebar, { Metadata } from './Sidebar';
import { HasuraMetadataV3 } from '../../../metadata/types';
import { ConsoleTypeDecorator } from '../../../storybook/decorators';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      cacheTime: 0,
    },
  },
});
const baseUrl = 'http://localhost:8080';

const generateArgs: (metadataOk?: boolean) => {
  location: RouteComponentProps<unknown, unknown>['location'];
  metadata: Metadata;
} = (metadataOk = true) => ({
  location: {
    action: 'POP',
    hash: '',
    key: '5nvxpbdafa',
    pathname: '/settings/metadata-actions',
    search: '',
    state: undefined,
    query: {},
  },
  metadata: metadataOk
    ? {
        inconsistentInheritedRoles: [],
        inconsistentObjects: [],
      }
    : {
        inconsistentInheritedRoles: [{ key: '123' }],
        inconsistentObjects: [],
      },
});

const mockHandlers = ({
  delay = 1,
  status = 200,
  prometheusEnabled = false,
  openTelemetryEnabled = false,
}: {
  delay?: number | DelayMode;
  status?: number;
  prometheusEnabled?: boolean;
  openTelemetryEnabled?: boolean;
}) => {
  return [
    rest.get(`${baseUrl}/v1alpha1/config`, (req, res, ctx) => {
      return res(
        ctx.status(status),
        ctx.delay(delay),
        ctx.json({
          version: '12345',
          is_function_permissions_inferred: true,
          is_remote_schema_permissions_enabled: false,
          is_admin_secret_set: false,
          is_auth_hook_set: false,
          is_jwt_set: false,
          jwt: [],
          is_allow_list_enabled: false,
          live_queries: {
            batch_size: 100,
            refetch_delay: 1,
          },
          streaming_queries: {
            batch_size: 100,
            refetch_delay: 1,
          },
          console_assets_dir:
            '/home/alex/src/graphql-engine-mono/console/static/dist',
          experimental_features: [],
          is_prometheus_metrics_enabled: prometheusEnabled,
        })
      );
    }),
    rest.post(`${baseUrl}/v1/metadata`, (req, res, ctx) => {
      let result: HasuraMetadataV3 = {
        version: 3,
        sources: [],
        inherited_roles: [],
      };
      if (openTelemetryEnabled) {
        result = {
          ...result,
          opentelemetry: {
            status: 'enabled',
            exporter_otlp: {
              headers: [],
              protocol: 'http/protobuf',
              resource_attributes: [],
              otlp_traces_endpoint: '',
            },
            data_types: [],
            batch_span_processor: {
              max_export_batch_size: 0,
            },
          },
        };
      } else {
        result = {
          ...result,
          opentelemetry: {
            status: 'disabled',
            exporter_otlp: {
              headers: [],
              protocol: 'http/protobuf',
              resource_attributes: [],
              otlp_traces_endpoint: '',
            },
            data_types: [],
            batch_span_processor: {
              max_export_batch_size: 0,
            },
          },
        };
      }
      return res(
        ctx.status(status),
        ctx.delay(delay),
        ctx.json({ metadata: result })
      );
    }),
  ];
};

export default {
  title: 'Features/Settings/Sidebar',
  component: Sidebar,
  parameters: {
    docs: { disable: true },
  },
  decorators: [
    (Story: React.FC) => (
      <div className="max-w-xs">
        <QueryClientProvider client={queryClient}>
          <Story />
        </QueryClientProvider>
      </div>
    ),
  ],
} as Meta<typeof Sidebar>;

export const MetadataOk: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Metadata Ok',
  args: generateArgs(),

  parameters: {
    msw: [...mockHandlers({}), eeLicenseInfo.active],
  },
};

export const MetadataKo: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Metadata Ko',
  args: generateArgs(false),

  parameters: {
    msw: [...mockHandlers({}), eeLicenseInfo.active],
  },
};

export const LogoutActive: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Logout Active',
  args: generateArgs(),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro', adminSecret: true })],
  parameters: {
    msw: [...mockHandlers({}), eeLicenseInfo.active],
  },
};

export const ProLiteLoading: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Lite Prometheus Loading',
  args: generateArgs(),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: mockHandlers({ delay: 'infinite' }),
  },
};

export const ProLitePrometheusWithoutLicense: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Lite Prometheus Without License',
  args: generateArgs(),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [...mockHandlers({ prometheusEnabled: true }), eeLicenseInfo.none],
  },
};

export const ProLitePrometheusEnabled: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Lite Prometheus Enabled',
  args: generateArgs(),

  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [...mockHandlers({ prometheusEnabled: true }), eeLicenseInfo.active],
  },
};

export const ProLitePrometheusDisabled: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Lite Prometheus Disabled',
  args: generateArgs(),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [...mockHandlers({ prometheusEnabled: false }), eeLicenseInfo.active],
  },
};

export const ProLiteError: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Lite Prometheus Error',
  args: generateArgs(),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [...mockHandlers({ status: 500 }), eeLicenseInfo.active],
  },
};

export const ProLiteOpenTelemetryWithoutLicense: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Lite OpenTelemetry Without License',
  args: generateArgs(),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [...mockHandlers({ openTelemetryEnabled: false }), eeLicenseInfo.none],
  },
};

export const ProLiteOpenTelemetryEnabled: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Lite OpenTelemetry Enabled',
  args: generateArgs(),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [
      ...mockHandlers({ openTelemetryEnabled: true }),
      eeLicenseInfo.active,
    ],
  },
};

export const ProLiteOpenTelemetryDisabled: StoryObj<typeof Sidebar> = {
  render: args => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Pro Lite OpenTelemetry Disabled',
  args: generateArgs(),
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: [
      ...mockHandlers({ openTelemetryEnabled: false }),
      eeLicenseInfo.active,
    ],
  },
};
