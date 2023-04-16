import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';

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
} as ComponentMeta<typeof PrometheusSettingsForm>;

export const Loading: ComponentStory<typeof PrometheusSettingsForm> = args => (
  <PrometheusSettingsForm {...args} />
);
Loading.storyName = '💠 Demo Form Loading';
Loading.args = {
  loading: true,
  eeLiteAccess: eeLiteAccessInfoMockActive,
};

export const DisabledWithoutLicense: ComponentStory<
  typeof PrometheusSettingsForm
> = args => <PrometheusSettingsForm {...args} />;
DisabledWithoutLicense.storyName = '💠 Demo Form Disabled without license';
DisabledWithoutLicense.args = {
  enabled: false,
  eeLiteAccess: eeLiteAccessInfoMockEligible,
};

export const Disabled: ComponentStory<typeof PrometheusSettingsForm> = args => (
  <PrometheusSettingsForm {...args} />
);
Disabled.storyName = '💠 Demo Form Disabled';
Disabled.args = {
  enabled: false,
  eeLiteAccess: eeLiteAccessInfoMockActive,
};

export const Error: ComponentStory<typeof PrometheusSettingsForm> = args => (
  <PrometheusSettingsForm {...args} />
);
Error.storyName = '💠 Demo Form Error';
Error.args = {
  errorMode: true,
  eeLiteAccess: eeLiteAccessInfoMockActive,
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
};
