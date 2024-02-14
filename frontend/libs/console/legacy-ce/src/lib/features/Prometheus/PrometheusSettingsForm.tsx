import React from 'react';
import { z } from 'zod';
import Skeleton from 'react-loading-skeleton';
import 'ace-builds/src-noconflict/mode-yaml';

import {
  CodeEditorField,
  InputField,
  SimpleForm,
} from '../../new-components/Form';
import { Button } from '../../new-components/Button';
import { Badge } from '../../new-components/Badge';
import { Card } from '../../new-components/Card';
import { LearnMoreLink } from '../../new-components/LearnMoreLink';
import {
  FaCheckCircle,
  FaExclamationTriangle,
  FaTimesCircle,
} from 'react-icons/fa';
import { PrometheusAnimation } from './PrometheusAnimation';
import { EETrialCard, EELiteAccess } from '../EETrial';

type PrometheusFormProps = {
  /**
   * Flag indicating whether the form is loading
   */
  loading?: boolean;
  /**
   * Flag indicating whether the form is enabled
   */
  enabled?: boolean;
  /**
   * The Prometheus URL
   */
  prometheusUrl?: string;
  /**
   * The Prometheus config
   */
  prometheusConfig?: string;
  /**
   * Flag indicating whether the form should display error mode
   */
  errorMode: boolean;
  /**
   * Flag indicating whether a EETrial license is activated
   */
  eeLiteAccess: EELiteAccess;
};

const PrometheusFormIntro = () => (
  <p className="text-muted">
    Expose your Prometheus performance metrics from your Hasura GraphQL Engine.
    <LearnMoreLink href="https://hasura.io/docs/latest/enterprise/metrics/" />
  </p>
);

type PrometheusFormFieldsProps = {
  loading?: boolean;
  prometheusUrl?: string;
  prometheusConfig?: string;
};

const PrometheusFormFields = ({
  loading,
  prometheusUrl,
  prometheusConfig,
}: PrometheusFormFieldsProps) => (
  <SimpleForm
    schema={z.object({})}
    onSubmit={() => {}}
    options={{
      defaultValues: {
        prometheusUrl,
        prometheusConfig,
      },
    }}
  >
    <>
      <InputField
        name="prometheusUrl"
        label="Prometheus URL"
        placeholder="URL"
        tooltip="This is the URL from which Hasura exposes its metrics in the Prometheus format."
        loading={loading}
        size="full"
        disabled
      />
      <CodeEditorField
        name="prometheusConfig"
        label="Example Prometheus Configuration (.yml)"
        tooltip={
          <span>
            This is a{' '}
            <span className="font-mono text-sm w-max text-red-600 bg-red-50 px-1.5 py-0.5 rounded">
              scrape_config
            </span>{' '}
            section of{' '}
            <a
              href="https://prometheus.io/docs/prometheus/latest/configuration/configuration/#scrape_config"
              target="_blank"
              rel="noreferrer"
              className="text-cloud italic"
            >
              a Prometheus configuration file
            </a>{' '}
            used for scraping metrics from this Hasura instance.
          </span>
        }
        mode="yaml"
        theme="eclipse"
        editorOptions={{
          minLines: 17,
          maxLines: 20,
        }}
        loading={loading}
        size="full"
        disabled
      />
    </>
  </SimpleForm>
);

PrometheusFormFields.defaultProps = {
  loading: false,
  prometheusUrl: '',
  prometheusConfig: '',
};

const PrometheusInstructionsCard = () => (
  <Card mode="neutral">
    <div className="mb-sm">
      <p className="p-0">
        To enable Prometheus metrics and to secure its endpoint, please set the
        environment variables below:
      </p>
      <p className="p-0">
        -{' '}
        <span className="font-mono text-sm w-max text-red-600 bg-red-50 px-1.5 py-0.5 rounded">
          HASURA_GRAPHQL_ENABLED_APIS=metadata,graphql,config,metrics
        </span>{' '}
        to enable Prometheus metrics,
      </p>
      <p className="p-0">
        -{' '}
        <span className="font-mono text-sm w-max text-red-600 bg-red-50 px-1.5 py-0.5 rounded">
          HASURA_GRAPHQL_METRICS_SECRET=[secret]
        </span>{' '}
        to secure your endpoint with a secret.
      </p>
    </div>
    <p className="p-0">
      For more information on which metrics are exported and how to enable them,
      see{' '}
      <a
        className="text-secondary hover:text-secondary-dark"
        target="_new"
        href="https://hasura.io/docs/latest/enterprise/metrics/"
      >
        Enabling Prometheus Metrics
      </a>
    </p>
  </Card>
);

const PrometheusErrorCard = () => (
  <Card mode="error">
    <div className="mb-sm">
      <p className="font-semibold">Could not retrieve status</p>
      <p className="p-0">
        There was an error retrieving the data required to display your
        Prometheus settings. Please try retry loading your settings for your
        Prometheus status.
      </p>
    </div>
    <div>
      <Button
        mode="primary"
        onClick={() => {
          window.location.reload();
        }}
      >
        Retry Loading Prometheus
      </Button>
    </div>
  </Card>
);

export const PrometheusSettingsForm: React.VFC<PrometheusFormProps> = ({
  loading = false,
  enabled = false,
  prometheusUrl = '',
  prometheusConfig = '',
  errorMode = false,
  eeLiteAccess,
}) => {
  let PrometheusBadge = () => <></>;
  let PrometheusSettings = () => <></>;

  const withoutLicense = eeLiteAccess.access !== 'active';

  if (loading) {
    PrometheusBadge = () => <Skeleton className="w-28 h-5" />;
    PrometheusSettings = () => (
      <>
        <Skeleton className="w-full h-[226px]" />
        <Skeleton className="w-full h-[148px] mt-md" />
      </>
    );
  } else if (errorMode && !withoutLicense) {
    PrometheusBadge = () => (
      <Badge color="red" className="flex gap-2">
        <FaExclamationTriangle />
        Error Retrieving Status
      </Badge>
    );
    PrometheusSettings = () => (
      <>
        <PrometheusAnimation errorMode />
        <PrometheusErrorCard />
      </>
    );
  } else if (enabled && !withoutLicense) {
    PrometheusBadge = () => (
      <Badge color="green" className="flex gap-2">
        <FaCheckCircle />
        Enabled
      </Badge>
    );
    PrometheusSettings = () => (
      <>
        <PrometheusAnimation enabled />
        <PrometheusFormFields
          prometheusUrl={prometheusUrl}
          prometheusConfig={prometheusConfig}
        />
      </>
    );
  } else {
    PrometheusBadge = () => (
      <Badge color="gray" className="flex gap-2">
        <FaTimesCircle />
        Disabled
      </Badge>
    );
    PrometheusSettings = () => (
      <>
        <PrometheusAnimation />
        {eeLiteAccess.access !== 'active' ? (
          <EETrialCard
            id="prometheus-settings"
            cardTitle="Gain visibility into your API performance with Prometheus metrics collection"
            cardText={
              <span>
                Collect, store and query for time-series metrics for your API to
                provide you with actionable insights and alerting capabilities
                so you can optimize performance and troubleshoot issues in
                real-time.
              </span>
            }
            buttonLabel="Enable Enterprise"
            horizontal
            eeAccess={eeLiteAccess.access}
          />
        ) : (
          <PrometheusInstructionsCard />
        )}
      </>
    );
  }

  return (
    <div className="space-y-md max-w-screen-md p-md">
      <div>
        <div className="flex items-center gap-4">
          <h1 className="text-xl font-semibold">Prometheus Metrics</h1>
          <PrometheusBadge />
        </div>
        <PrometheusFormIntro />
      </div>
      <PrometheusSettings />
    </div>
  );
};
