import React from 'react';

import { useServerConfig } from '../../hooks';
import endpoints from '../../Endpoints';
import { PrometheusSettingsForm } from './PrometheusSettingsForm';
import globals from '../../Globals';
import { useEELiteAccess } from '../../features/EETrial';

export const extractPrometheusUrl = (prometheusUrl: string) => {
  const urlRegExp =
    /^(https?):\/\/(?:www\.)?([-a-zA-Z0-9@:%._\+~#=]{1,256}\.?[a-zA-Z0-9()]{0,6})\b(?:[-a-zA-Z0-9()@:%_\+.~#?&\/=]*)$/;
  let prometheusUrlExtract = prometheusUrl.match(urlRegExp);
  if (!prometheusUrlExtract) {
    prometheusUrlExtract = ['', 'http', 'myhasuradomain.com'];
  }
  return prometheusUrlExtract;
};

export const PrometheusSettings: React.VFC<Record<string, never>> = () => {
  const eeLiteAccess = useEELiteAccess(globals);
  const { data: configData, isLoading } = useServerConfig();

  // eslint-disable-next-line no-underscore-dangle
  if (eeLiteAccess.access === 'forbidden') {
    return null;
  }

  const prometheusUrlExtract = extractPrometheusUrl(endpoints.prometheusUrl);
  return (
    <PrometheusSettingsForm
      loading={isLoading || eeLiteAccess.access === 'loading'}
      enabled={configData?.is_prometheus_metrics_enabled}
      errorMode={false}
      prometheusUrl={endpoints.prometheusUrl}
      prometheusConfig={`global:
  scrape_interval: 60s
scrape_configs:
  - job_name: 'hasura'
    scrape_interval: 60s ## Recommended scrape interval is 60s
    metrics_path: '/v1/metrics'
    scheme: '${prometheusUrlExtract[1]}'
    authorization:
      ## Replace with the secret set as per env variable HASURA_GRAPHQL_METRICS_SECRET
      credentials: '<my_prometheus_secret>'
    static_configs:
      - targets: ['${prometheusUrlExtract[2]}']
        ## Add extra labels here
        labels:
          service_name: hasura
          container_id: hasura`}
      eeLiteAccess={eeLiteAccess}
    />
  );
};
