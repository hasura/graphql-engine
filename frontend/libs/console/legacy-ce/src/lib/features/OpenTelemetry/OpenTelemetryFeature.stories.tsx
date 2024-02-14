import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { rest, DelayMode } from 'msw';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';

import { OpenTelemetryFeature } from './OpenTelemetryFeature';
import { eeLicenseInfo } from '../EETrial/mocks/http';
import { registerEETrialLicenseActiveMutation } from '../EETrial/mocks/registration.mock';
import { HasuraMetadataV3 } from '../../metadata/types';
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
