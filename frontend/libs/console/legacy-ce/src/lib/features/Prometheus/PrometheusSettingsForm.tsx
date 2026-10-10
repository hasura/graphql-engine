import React from 'react';
import { z } from 'zod';
import {
  CodeEditorField,
  InputField,
  SimpleForm,
  Button,
  Badge,
  Card,
  LearnMoreLink,
  Text,
} from '@hasura/shared/ui';
import {
  FaCheckCircle,
  FaExclamationTriangle,
  FaTimesCircle,
} from 'react-icons/fa';
import { PrometheusAnimation } from './PrometheusAnimation';
import { EETrialCard, EELiteAccess } from '../EETrial';
import { Code, Flex, Heading, Link, Skeleton } from '@radix-ui/themes';

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
    schema={z.object({
      prometheusUrl: z.string().optional(),
      prometheusConfig: z.string().optional(),
    })}
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
        fieldProps={{ placeholder: 'URL', disabled: true }}
        tooltip="This is the URL from which Hasura exposes its metrics in the Prometheus format."
        loading={loading}
        size="full"
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
        editorProps={{
          mode: 'yaml',
        }}
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

const PrometheusInstructionsCard = () => (
  <Card mode="neutral">
    <div className="mb-2">
      <Text as="p">
        To enable Prometheus metrics and to secure its endpoint, please set the
        environment variables below:
      </Text>
      <Text as="p">
        -{' '}
        <Code color="red">
          HASURA_GRAPHQL_ENABLED_APIS=metadata,graphql,config,metrics
        </Code>{' '}
        to enable Prometheus metrics,
      </Text>
      <Text as="p">
        - <Code color="red">HASURA_GRAPHQL_METRICS_SECRET=[secret]</Code> to
        secure your endpoint with a secret.
      </Text>
    </div>
    <Text as="p">
      For more information on which metrics are exported and how to enable them,
      see{' '}
      <Link
        target="_blank"
        href="https://hasura.io/docs/latest/enterprise/metrics/"
      >
        Enabling Prometheus Metrics
      </Link>
    </Text>
  </Card>
);

const PrometheusErrorCard = () => (
  <Card mode="error" size="3">
    <div className="mb-2">
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

const renderPrometheusBadge = ({
  loading,
  errorMode,
  enabled,
  withoutLicense,
}: {
  loading: boolean;
  errorMode: boolean;
  enabled: boolean;
  withoutLicense: boolean;
}) => {
  if (loading) {
    return <Skeleton width="100px" height="1.25rem" />;
  }

  if (errorMode && !withoutLicense) {
    return (
      <Badge color="red" className="flex gap-2">
        <FaExclamationTriangle />
        Error Retrieving Status
      </Badge>
    );
  }
  if (enabled && !withoutLicense) {
    return (
      <Badge color="green" className="flex gap-2">
        <FaCheckCircle />
        Enabled
      </Badge>
    );
  }
  return (
    <Badge color="gray" className="flex gap-2">
      <FaTimesCircle />
      Disabled
    </Badge>
  );
};

const renderPrometheusSettings = ({
  loading,
  errorMode,
  enabled,
  withoutLicense,
  prometheusUrl,
  prometheusConfig,
  eeLiteAccess,
}: {
  loading: boolean;
  errorMode: boolean;
  enabled: boolean;
  withoutLicense: boolean;
  prometheusUrl: string;
  prometheusConfig: string;
  eeLiteAccess: EELiteAccess;
}) => {
  if (loading) {
    return (
      <>
        <Skeleton width="100%" height="226px" />
        <Skeleton width="100%" height="148px" />
      </>
    );
  }
  if (errorMode && !withoutLicense) {
    return (
      <>
        <PrometheusAnimation errorMode />
        <PrometheusErrorCard />
      </>
    );
  }
  if (enabled && !withoutLicense) {
    return (
      <>
        <PrometheusAnimation enabled />
        <PrometheusFormFields
          prometheusUrl={prometheusUrl}
          prometheusConfig={prometheusConfig}
        />
      </>
    );
  }
  return (
    <>
      <PrometheusAnimation />
      {eeLiteAccess.access !== 'active' ? (
        <EETrialCard
          id="prometheus-settings"
          cardTitle="Gain visibility into your API performance with Prometheus metrics collection"
          cardText={
            <Text>
              Collect, store and query for time-series metrics for your API to
              provide you with actionable insights and alerting capabilities so
              you can optimize performance and troubleshoot issues in real-time.
            </Text>
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
};

export const PrometheusSettingsForm: React.FC<PrometheusFormProps> = ({
  loading = false,
  enabled = false,
  prometheusUrl = '',
  prometheusConfig = '',
  errorMode = false,
  eeLiteAccess,
}) => {
  const withoutLicense = eeLiteAccess.access !== 'active';

  return (
    <Flex direction="column" gap="4" className="max-w-(--breakpoint-lg) p-4">
      <Flex align="baseline" gap="4">
        <Heading size="6">Prometheus Metrics</Heading>
        {renderPrometheusBadge({
          loading,
          errorMode,
          enabled,
          withoutLicense,
        })}
      </Flex>
      <Text as="p">
        Expose your Prometheus performance metrics from your Hasura GraphQL
        Engine.{' '}
        <LearnMoreLink href="https://hasura.io/docs/latest/enterprise/metrics/" />
      </Text>
      {renderPrometheusSettings({
        loading,
        errorMode,
        enabled,
        withoutLicense,
        prometheusUrl,
        prometheusConfig,
        eeLiteAccess,
      })}
    </Flex>
  );
};
