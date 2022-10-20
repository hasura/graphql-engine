import React from 'react';
import { z } from 'zod';
import Skeleton from 'react-loading-skeleton';
import 'ace-builds/src-noconflict/mode-yaml';

import { Form, InputField, CodeEditorField } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { Badge } from '@/new-components/Badge';
import { Card } from '@/new-components/Card';
import {
  FaCheckCircle,
  FaTimesCircle,
  FaExclamationTriangle,
} from 'react-icons/fa';
import { PrometheusAnimation } from './PrometheusAnimation';

type PrometheusFormProps = {
  /**
   * Flag indicating wheter the form is loading
   */
  loading?: boolean;
  /**
   * Flag indicating wheter the form is enabled
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
   * Flag indicating wheter the form should display error mode
   */
  errorMode: boolean;
};

const PrometheusFormIntro = () => (
  <p className="text-muted">
    Expose your Prometheus performance metrics from your Hasura GraphQL Engine.{' '}
    <a
      href="https://hasura.io/docs/latest/enterprise/metrics/"
      className="text-secondary text-sm italic hover:underline hover:cursor-pointer"
    >
      (Read More)
    </a>
  </p>
);

type PrometheidFormFieldsProps = {
  loading?: boolean;
  prometheusUrl?: string;
  prometheusConfig?: string;
};

const PrometheusFormFields = ({
  loading,
  prometheusUrl,
  prometheusConfig,
}: PrometheidFormFieldsProps) => (
  <Form
    schema={z.object({})}
    onSubmit={() => {}}
    options={{
      defaultValues: {
        prometheusUrl,
        prometheusConfig,
      },
    }}
  >
    {() => (
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
    )}
  </Form>
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
}) => {
  let PrometheusBadge = () => <></>;
  let PrometheusSettings = () => <></>;

  if (loading) {
    PrometheusBadge = () => <Skeleton className="w-28 h-5" />;
    PrometheusSettings = () => (
      <>
        <Skeleton className="w-full h-[226px]" />
        <PrometheusFormFields loading />
      </>
    );
  } else if (errorMode) {
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
  } else if (enabled) {
    PrometheusBadge = () => (
      <Badge color="green" className="flex gap-2">
        <FaCheckCircle />
        Enabled
      </Badge>
    );
    PrometheusSettings = () => (
      <>
        <PrometheusAnimation />
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
        <PrometheusInstructionsCard />
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
