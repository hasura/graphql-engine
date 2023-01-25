import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';

import { PrometheusSettingsForm } from './PrometheusSettingsForm';

export default {
  title: 'Features/Settings/Prometheus/Form',
  component: PrometheusSettingsForm,
  parameters: {
    docs: {
      source: { type: 'code', state: 'open' },
    },
  },
} as ComponentMeta<typeof PrometheusSettingsForm>;

export const Loading: ComponentStory<typeof PrometheusSettingsForm> = args => (
  <PrometheusSettingsForm {...args} />
);
Loading.storyName = '💠 Demo Form Loading';
Loading.args = {
  loading: true,
};

export const Disabled: ComponentStory<typeof PrometheusSettingsForm> = args => (
  <PrometheusSettingsForm {...args} />
);
Disabled.storyName = '💠 Demo Form Disabled';
Disabled.args = {
  enabled: false,
};

export const Error: ComponentStory<typeof PrometheusSettingsForm> = args => (
  <PrometheusSettingsForm {...args} />
);
Error.storyName = '💠 Demo Form Error';
Error.args = {
  errorMode: true,
};

const urlRegExp =
  /^(https?):\/\/(?:www\.)?([-a-zA-Z0-9@:%._\+~#=]{1,256}\.?[a-zA-Z0-9()]{0,6})\b(?:[-a-zA-Z0-9()@:%_\+.~#?&\/=]*)$/;
const prometheusUrl = 'https://mydomain.io:8080/v1/metrics';
const prometheusUrlExtract = prometheusUrl.match(urlRegExp);
export const Enabled: ComponentStory<typeof PrometheusSettingsForm> = args => (
  <PrometheusSettingsForm {...args} />
);
Enabled.storyName = '💠 Demo Form Enabled';
Enabled.args = {
  enabled: true,
  prometheusUrl,
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
};
