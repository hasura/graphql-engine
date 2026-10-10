import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { http, HttpResponse, delay, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';

import { eeLicenseInfo } from '../../../features/EETrial/mocks/http';
import Sidebar from './Sidebar';
import { ConsoleTypeDecorator } from '@hasura/shared/testing';
import { HasuraMetadataV3 } from '@hasura/shared/types';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      gcTime: 0,
    },
  },
});
const baseUrl = 'http://localhost:8080';

const generateArgs: (metadataOk?: boolean) => {} = (metadataOk = true) => ({
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
  delay: delayOpt = 1,
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
    http.get(`${baseUrl}/v1alpha1/config`, async () => {
      await delay(delayOpt);
      return HttpResponse.json(
        {
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
        },
        { status },
      );
    }),
    http.post(`${baseUrl}/v1/metadata`, async () => {
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
              traces_propagators: ['b3'],
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
              traces_propagators: ['b3'],
            },
            data_types: [],
            batch_span_processor: {
              max_export_batch_size: 0,
            },
          },
        };
      }
      await delay(delayOpt);
      return HttpResponse.json({ metadata: result }, { status });
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
  render: (args) => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Metadata Ok',
  args: generateArgs(),

  parameters: {
    msw: [...mockHandlers({}), eeLicenseInfo.active],
  },
};

export const MetadataKo: StoryObj<typeof Sidebar> = {
  render: (args) => {
    return <Sidebar {...args} />;
  },

  name: '💠 Demo Metadata Ko',
  args: generateArgs(false),

  parameters: {
    msw: [...mockHandlers({}), eeLicenseInfo.active],
  },
};

export const LogoutActive: StoryObj<typeof Sidebar> = {
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
