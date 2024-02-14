import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { rest, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';

import { PrometheusSettings } from '.';
import { eeLicenseInfo } from '../EETrial/mocks/http';
import { registerEETrialLicenseActiveMutation } from '../EETrial/mocks/registration.mock';
import { ConsoleTypeDecorator } from '../../storybook/decorators';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      cacheTime: 0,
    },
  },
});
const baseUrl = 'http://localhost:8080';
// eslint-disable-next-line no-underscore-dangle
window.__env = {
  // eslint-disable-next-line no-underscore-dangle
  ...window.__env,
  dataApiUrl: baseUrl,
};

const mockConfigHandler = (
  prometheusEnabled: boolean,
  delay: number | DelayMode,
  status = 200
) => {
  return rest.get(`${baseUrl}/v1alpha1/config`, (req, res, ctx) => {
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
  });
};

export default {
  title: 'Features/Settings/Prometheus/Page',
  component: PrometheusSettings,
  parameters: {
    docs: { disable: true },
  },
  decorators: [
    (Story: React.FC) => (
      <QueryClientProvider client={queryClient}>
        <Story />
        <ReactQueryDevtools initialIsOpen={false} />
      </QueryClientProvider>
    ),
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
  ],
} as Meta<typeof PrometheusSettings>;

export const DisabledWithoutLicense: StoryObj<typeof PrometheusSettings> = {
  render: () => <PrometheusSettings />,
  name: '💠 Demo Page Disabled without license',

  parameters: {
    msw: [
      mockConfigHandler(false, 1),
      eeLicenseInfo.noneOnce,
      registerEETrialLicenseActiveMutation,
      eeLicenseInfo.active,
    ],
  },
};

export const Loading: StoryObj<typeof PrometheusSettings> = {
  render: () => <PrometheusSettings />,

  name: '💠 Demo Page Loading',

  parameters: {
    msw: [mockConfigHandler(true, 'infinite'), eeLicenseInfo.active],
  },
};

export const Enabled: StoryObj<typeof PrometheusSettings> = {
  render: () => <PrometheusSettings />,

  name: '💠 Demo Page Enabled',

  parameters: {
    msw: [mockConfigHandler(true, 1), eeLicenseInfo.active],
  },
};

export const Disabled: StoryObj<typeof PrometheusSettings> = {
  render: () => <PrometheusSettings />,

  name: '💠 Demo Page Disabled',

  parameters: {
    msw: [mockConfigHandler(false, 1), eeLicenseInfo.active],
  },
};

export const Error: StoryObj<typeof PrometheusSettings> = {
  render: () => <PrometheusSettings />,

  name: '💠 Demo Page Error',

  parameters: {
    msw: [mockConfigHandler(false, 1, 500), eeLicenseInfo.active],
  },
};
