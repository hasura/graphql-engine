import React from 'react';
import { RouteComponentProps } from 'react-router';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import globals from '@/Globals';
import { rest, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from 'react-query';
import Sidebar, { Metadata } from './Sidebar';
import { HasuraMetadataV3 } from '@/metadata/types';

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
} as ComponentMeta<typeof Sidebar>;

export const MetadataOk: ComponentStory<typeof Sidebar> = args => {
  return <Sidebar {...args} />;
};
MetadataOk.storyName = '💠 Demo Metadata Ok';
MetadataOk.args = generateArgs();
MetadataOk.parameters = {
  msw: mockHandlers({}),
};

export const MetadataKo: ComponentStory<typeof Sidebar> = args => {
  return <Sidebar {...args} />;
};
MetadataKo.storyName = '💠 Demo Metadata Ko';
MetadataKo.args = generateArgs(false);
MetadataKo.parameters = {
  msw: mockHandlers({}),
};

export const LogoutActive: ComponentStory<typeof Sidebar> = args => {
  return <Sidebar {...args} />;
};
LogoutActive.storyName = '💠 Demo Pro Logout Active';
LogoutActive.args = generateArgs();
LogoutActive.parameters = {
  msw: mockHandlers({}),
  adminSecretSet: true,
};

export const ProLiteLoading: ComponentStory<typeof Sidebar> = args => {
  return <Sidebar {...args} />;
};
ProLiteLoading.storyName = '💠 Demo Pro Lite Prometheus Loading';
ProLiteLoading.args = generateArgs();
ProLiteLoading.parameters = {
  msw: mockHandlers({ delay: 'infinite' }),
  consoleType: 'pro-lite',
};

export const ProLitePrometheusEnabled: ComponentStory<
  typeof Sidebar
> = args => {
  return <Sidebar {...args} />;
};
ProLitePrometheusEnabled.storyName = '💠 Demo Pro Lite Prometheus Enabled';
ProLitePrometheusEnabled.args = generateArgs();
ProLitePrometheusEnabled.parameters = {
  msw: mockHandlers({ prometheusEnabled: true }),
  consoleType: 'pro-lite',
};

export const ProLitePrometheusDisabled: ComponentStory<
  typeof Sidebar
> = args => {
  return <Sidebar {...args} />;
};
ProLitePrometheusDisabled.storyName = '💠 Demo Pro Lite Prometheus Disabled';
ProLitePrometheusDisabled.args = generateArgs();
ProLitePrometheusDisabled.parameters = {
  msw: mockHandlers({ prometheusEnabled: false }),
  consoleType: 'pro-lite',
};

export const ProLiteError: ComponentStory<typeof Sidebar> = args => {
  return <Sidebar {...args} />;
};
ProLiteError.storyName = '💠 Demo Pro Lite Prometheus Error';
ProLiteError.args = generateArgs();
ProLiteError.parameters = {
  msw: mockHandlers({ status: 500 }),
  consoleType: 'pro-lite',
};

export const ProLiteOpenTelemetryEnabled: ComponentStory<
  typeof Sidebar
> = args => {
  return <Sidebar {...args} />;
};
ProLiteOpenTelemetryEnabled.storyName =
  '💠 Demo Pro Lite OpenTelemetry Enabled';
ProLiteOpenTelemetryEnabled.args = generateArgs();
ProLiteOpenTelemetryEnabled.parameters = {
  msw: mockHandlers({ openTelemetryEnabled: true }),
  consoleType: 'pro-lite',
};

export const ProLiteOpenTelemetryDisabled: ComponentStory<
  typeof Sidebar
> = args => {
  return <Sidebar {...args} />;
};
ProLiteOpenTelemetryDisabled.storyName =
  '💠 Demo Pro Lite OpenTelemetry Disabled';
ProLiteOpenTelemetryDisabled.args = generateArgs();
ProLiteOpenTelemetryDisabled.parameters = {
  msw: mockHandlers({ openTelemetryEnabled: false }),
  consoleType: 'pro-lite',
};
