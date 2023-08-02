import { StoryObj, Meta } from '@storybook/react';

import { PrometheusSettingsForm } from './PrometheusSettingsForm';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { EELiteAccess } from '../EETrial';

const eeLiteAccessInfoMockActive: EELiteAccess = {
  access: 'active',
  kind: 'default',
  license: {} as any,
  expires_at: new Date(new Date().getTime() + 10000000),
};

const eeLiteAccessInfoMockEligible: EELiteAccess = {
  access: 'eligible',
};

export default {
  title: 'Features/Settings/Prometheus/Form',
  component: PrometheusSettingsForm,
  parameters: {
    docs: {
      source: { type: 'code', state: 'open' },
    },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof PrometheusSettingsForm>;

export const Loading: StoryObj<typeof PrometheusSettingsForm> = {
  name: '💠 Demo Form Loading',

  args: {
    loading: true,
    eeLiteAccess: eeLiteAccessInfoMockActive,
  },
};

export const DisabledWithoutLicense: StoryObj<typeof PrometheusSettingsForm> = {
  name: '💠 Demo Form Disabled without license',

  args: {
    enabled: false,
    eeLiteAccess: eeLiteAccessInfoMockEligible,
  },
};

export const Disabled: StoryObj<typeof PrometheusSettingsForm> = {
  name: '💠 Demo Form Disabled',

  args: {
    enabled: false,
    eeLiteAccess: eeLiteAccessInfoMockActive,
  },
};

export const Error: StoryObj<typeof PrometheusSettingsForm> = {
  name: '💠 Demo Form Error',

  args: {
    errorMode: true,
    eeLiteAccess: eeLiteAccessInfoMockActive,
  },
};

const urlRegExp =
  /^(https?):\/\/(?:www\.)?([-a-zA-Z0-9@:%._\+~#=]{1,256}\.?[a-zA-Z0-9()]{0,6})\b(?:[-a-zA-Z0-9()@:%_\+.~#?&\/=]*)$/;
const prometheusUrl = 'https://mydomain.io:8080/v1/metrics';
const prometheusUrlExtract = prometheusUrl.match(urlRegExp);

export const Enabled: StoryObj<typeof PrometheusSettingsForm> = {
  name: '💠 Demo Form Enabled',

  args: {
    enabled: true,
    prometheusUrl,
    eeLiteAccess: eeLiteAccessInfoMockActive,
    prometheusConfig: `global:
      scrape_interval: 60s
    scrape_configs:
      - job_name: 'hasura'
        scrape_interval: 60s ## Recommended scrape interval is 60s
        metrics_path: '/v1/metrics'
        scheme: '${(prometheusUrlExtract || [])[1]}'
        authorization:
          ## Replace with the secret set as per env variable HASURA_GRAPHQL_METRICS_SECRET
          credentials: '<my_prometheus_secret>' 
        static_configs:
          - targets: ['${(prometheusUrlExtract || [])[2]}']
            ## Add extra labels here
            labels:
              service_name: hasura
              container_id: hasura`,
  },
};
