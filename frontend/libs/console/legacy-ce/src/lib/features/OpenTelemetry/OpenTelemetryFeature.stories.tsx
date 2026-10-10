import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { http, HttpResponse, delay, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';

import { OpenTelemetryFeature } from './OpenTelemetryFeature';
import { eeLicenseInfo } from '../EETrial/mocks/http';
import { registerEETrialLicenseActiveMutation } from '../EETrial/mocks/registration.mock';
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

window.__env = {
  ...window.__env,
  dataApiUrl: baseUrl,
};

const mockMetadataHandler = (
  openTelemetryEnabled: boolean,
  delayOpt: number | DelayMode,
  status = 200,
) => {
  return http.post(`${baseUrl}/v1/metadata`, async () => {
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
            traces_propagators: [],
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
            traces_propagators: [],
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
    ConsoleTypeDecorator({ consoleType: 'pro-lite' }),
  ],
} as Meta<typeof OpenTelemetryFeature>;

export const DisabledWithoutLicense: StoryObj<typeof OpenTelemetryFeature> = {
  render: () => {
    return OpenTelemetryFeature() || <div></div>;
  },

  name: '💠 Demo Feature Disabled without license',

  parameters: {
    msw: [
      mockMetadataHandler(false, 1),
      eeLicenseInfo.noneOnce,
      registerEETrialLicenseActiveMutation,
      eeLicenseInfo.active,
    ],
  },
};

export const Loading: StoryObj<typeof OpenTelemetryFeature> = {
  render: () => {
    return OpenTelemetryFeature() || <div></div>;
  },

  name: '💠 Demo Feature Loading',

  parameters: {
    msw: [mockMetadataHandler(true, 'infinite'), eeLicenseInfo.active],
  },
};

export const Enabled: StoryObj<typeof OpenTelemetryFeature> = {
  render: () => {
    return OpenTelemetryFeature() || <div></div>;
  },

  name: '💠 Demo Feature Enabled',

  parameters: {
    msw: [mockMetadataHandler(true, 1), eeLicenseInfo.active],
  },
};

export const Disabled: StoryObj<typeof OpenTelemetryFeature> = {
  render: () => {
    return OpenTelemetryFeature() || <div></div>;
  },

  name: '💠 Demo Feature Disabled',

  parameters: {
    msw: [mockMetadataHandler(false, 1), eeLicenseInfo.active],
  },
};

export const Error: StoryObj<typeof OpenTelemetryFeature> = {
  render: () => {
    return OpenTelemetryFeature() || <div></div>;
  },

  name: '💠 Demo Feature Error',

  parameters: {
    msw: [mockMetadataHandler(false, 1, 500), eeLicenseInfo.active],
  },
};
