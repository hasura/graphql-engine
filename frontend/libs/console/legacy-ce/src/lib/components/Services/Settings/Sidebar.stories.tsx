import React from 'react';
import { RouteComponentProps } from 'react-router';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { rest, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from 'react-query';
import globals from '../../../Globals';

import Sidebar, { Metadata } from './Sidebar';

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
    pathname: '/settings',
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
}: {
  delay?: number | DelayMode;
  status?: number;
  prometheusEnabled?: boolean;
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
      <QueryClientProvider client={queryClient}>
        <Story />
      </QueryClientProvider>
    ),
  ],
} as ComponentMeta<typeof Sidebar>;

export const MetadataOk: ComponentStory<typeof Sidebar> = args => {
  globals.consoleType = 'pro';
  return <Sidebar {...args} />;
};
MetadataOk.storyName = '💠 Demo Metadata Ok';
MetadataOk.args = generateArgs();
MetadataOk.parameters = {
  msw: mockHandlers({}),
};

export const MetadataKo: ComponentStory<typeof Sidebar> = args => {
  globals.consoleType = 'pro';
  return <Sidebar {...args} />;
};
MetadataKo.storyName = '💠 Demo Metadata Ko';
MetadataKo.args = generateArgs(false);
MetadataKo.parameters = {
  msw: mockHandlers({}),
};

export const CloudLoading: ComponentStory<typeof Sidebar> = args => {
  globals.consoleType = 'cloud';
  return <Sidebar {...args} />;
};
CloudLoading.storyName = '💠 Demo Cloud Loading';
CloudLoading.args = generateArgs();
CloudLoading.parameters = {
  msw: mockHandlers({ delay: 'infinite' }),
};

export const CloudMetadataOk: ComponentStory<typeof Sidebar> = args => {
  globals.consoleType = 'cloud';
  return <Sidebar {...args} />;
};
CloudMetadataOk.storyName = '💠 Demo Cloud Metadata Ok';
CloudMetadataOk.args = generateArgs();
CloudMetadataOk.parameters = {
  msw: mockHandlers({}),
};

export const CloudMetadataKo: ComponentStory<typeof Sidebar> = args => {
  globals.consoleType = 'cloud';
  return <Sidebar {...args} />;
};
CloudMetadataKo.storyName = '💠 Demo Cloud Metadata Ko';
CloudMetadataKo.args = generateArgs(false);
CloudMetadataKo.parameters = {
  msw: mockHandlers({}),
};

export const CloudPrometheusEnabled: ComponentStory<typeof Sidebar> = args => {
  globals.consoleType = 'cloud';
  return <Sidebar {...args} />;
};
CloudPrometheusEnabled.storyName = '💠 Demo Cloud Prometheus Enabled';
CloudPrometheusEnabled.args = generateArgs();
CloudPrometheusEnabled.parameters = {
  msw: mockHandlers({ prometheusEnabled: true }),
};

export const CloudPrometheusDisabled: ComponentStory<typeof Sidebar> = args => {
  globals.consoleType = 'cloud';
  return <Sidebar {...args} />;
};
CloudPrometheusDisabled.storyName = '💠 Demo Cloud Prometheus Disabled';
CloudPrometheusDisabled.args = generateArgs();
CloudPrometheusDisabled.parameters = {
  msw: mockHandlers({ prometheusEnabled: false }),
};

export const CloudError: ComponentStory<typeof Sidebar> = args => {
  globals.consoleType = 'cloud';
  return <Sidebar {...args} />;
};
CloudError.storyName = '💠 Demo Cloud Error';
CloudError.args = generateArgs();
CloudError.parameters = {
  msw: mockHandlers({ status: 500 }),
};
