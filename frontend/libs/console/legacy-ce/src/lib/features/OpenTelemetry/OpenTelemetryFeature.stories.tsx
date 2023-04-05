import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { rest, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';

import { OpenTelemetryFeature } from './OpenTelemetryFeature';
import { eeLicenseInfo } from '../EETrial/mocks/http';
import { registerEETrialLicenseActiveMutation } from '../EETrial/mocks/registration.mock';
import { HasuraMetadataV3 } from '../../metadata/types';

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

const mockMetadataHandler = (
  openTelemetryEnabled: boolean,
  delay: number | DelayMode,
  status = 200
) => {
  return rest.post(`${baseUrl}/v1/metadata`, (req, res, ctx) => {
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
  });
};

export default {
  title: 'Features/OpenTelemetry/Feature',
  component: OpenTelemetryFeature,
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
  ],
} as ComponentMeta<typeof OpenTelemetryFeature>;

export const DisabledWithoutLicense: ComponentStory<
  typeof OpenTelemetryFeature
> = () => {
  return OpenTelemetryFeature() || <div></div>;
};
DisabledWithoutLicense.storyName = '💠 Demo Feature Disabled without license';
DisabledWithoutLicense.parameters = {
  msw: [
    mockMetadataHandler(false, 1),
    eeLicenseInfo.noneOnce,
    registerEETrialLicenseActiveMutation,
    eeLicenseInfo.active,
  ],
  consoleType: 'pro-lite',
};

export const Loading: ComponentStory<typeof OpenTelemetryFeature> = () => {
  return OpenTelemetryFeature() || <div></div>;
};
Loading.storyName = '💠 Demo Feature Loading';
Loading.parameters = {
  msw: [mockMetadataHandler(true, 'infinite'), eeLicenseInfo.active],
  consoleType: 'pro-lite',
};

export const Enabled: ComponentStory<typeof OpenTelemetryFeature> = () => {
  return OpenTelemetryFeature() || <div></div>;
};
Enabled.storyName = '💠 Demo Feature Enabled';
Enabled.parameters = {
  msw: [mockMetadataHandler(true, 1), eeLicenseInfo.active],
  consoleType: 'pro-lite',
};

export const Disabled: ComponentStory<typeof OpenTelemetryFeature> = () => {
  return OpenTelemetryFeature() || <div></div>;
};
Disabled.storyName = '💠 Demo Feature Disabled';
Disabled.parameters = {
  msw: [mockMetadataHandler(false, 1), eeLicenseInfo.active],
  consoleType: 'pro-lite',
};

export const Error: ComponentStory<typeof OpenTelemetryFeature> = () => {
  return OpenTelemetryFeature() || <div></div>;
};
Error.storyName = '💠 Demo Feature Error';
Error.parameters = {
  msw: [mockMetadataHandler(false, 1, 500), eeLicenseInfo.active],
  consoleType: 'pro-lite',
};
