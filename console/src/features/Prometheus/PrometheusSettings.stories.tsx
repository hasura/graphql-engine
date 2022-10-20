import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { rest, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from 'react-query';

import { PrometheusSettings } from '@/features/Prometheus';

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

const mockHandler = (
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
      </QueryClientProvider>
    ),
  ],
} as ComponentMeta<typeof PrometheusSettings>;

export const Loading: ComponentStory<typeof PrometheusSettings> = () => (
  <PrometheusSettings />
);
Loading.storyName = '💠 Demo Page Loading';
Loading.parameters = {
  msw: [mockHandler(true, 'infinite')],
};

export const Enabled: ComponentStory<typeof PrometheusSettings> = () => (
  <PrometheusSettings />
);
Enabled.storyName = '💠 Demo Page Enabled';
Enabled.parameters = {
  msw: [mockHandler(true, 1)],
};

export const Disabled: ComponentStory<typeof PrometheusSettings> = () => (
  <PrometheusSettings />
);
Disabled.storyName = '💠 Demo Page Disabled';
Disabled.parameters = {
  msw: [mockHandler(false, 1)],
};

export const Error: ComponentStory<typeof PrometheusSettings> = () => (
  <PrometheusSettings />
);
Error.storyName = '💠 Demo Page Error';
Error.parameters = {
  msw: [mockHandler(false, 1, 500)],
};
