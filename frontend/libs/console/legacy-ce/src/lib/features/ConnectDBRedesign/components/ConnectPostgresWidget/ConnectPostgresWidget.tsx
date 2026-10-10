import { useEffect, useState } from 'react';
import { Flex, Heading } from '@radix-ui/themes';
import {
  hasuraToast,
  useConsoleForm,
  Tabs,
  Button,
  DisplayToastErrorMessage,
} from '@hasura/shared/ui';
import { useMetadata } from '@hasura/metadata/api';
import { PostgresSource, Source } from '@hasura/shared/types';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { LimitedFeatureWrapper } from '../LimitedFeatureWrapper/LimitedFeatureWrapper';
import { ConnectPostgresForm } from './parts/ConnectPostgresForm';
import { DynamicDBRouting } from './parts/DynamicDBRouting';
import { getDefaultValues, PostgresConnectionSchema, schema } from './schema';
import { generatePostgresRequestPayload } from './utils/generateRequests';

type PostgresLikeSource = Extract<
  Source,
  {
    kind: 'postgres' | 'citus' | 'cockroach';
  }
>;

const isPostgresLikeSource = (
  source: Source | undefined,
): source is PostgresLikeSource => {
  return (
    source?.kind === 'postgres' ||
    source?.kind === 'citus' ||
    source?.kind === 'cockroach'
  );
};

interface ConnectPostgresWidgetProps {
  dataSourceName?: string;

  //overrides for pg like sources;
  overrideDriver?: string;
  overrideDisplayName?: string;
}

export const ConnectPostgresWidget = (props: ConnectPostgresWidgetProps) => {
  const { dataSourceName, overrideDriver, overrideDisplayName } = props;
  const [tab, setTab] = useState('connectionDetails');

  const isEditMode = !!dataSourceName;

  const { data: metadataSource } = useMetadata((m) =>
    m.metadata.sources.find((source) => source.name === dataSourceName),
  );

  const { createConnection, editConnection, isPending } =
    useManageDatabaseConnection({
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: isEditMode
            ? 'Database updated successfully!'
            : 'Database added successfully!',
        });
      },
      onError: (err) => {
        hasuraToast({
          type: 'error',
          title: err.name,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });

  const handleSubmit = (formValues: PostgresConnectionSchema) => {
    const payload = generatePostgresRequestPayload({
      driver: overrideDriver ?? 'postgres',
      values: formValues,
      connectionTemplate: isPostgresLikeSource(metadataSource)
        ? (metadataSource as PostgresSource).configuration?.connection_template
        : undefined,
      connectionSet: isPostgresLikeSource(metadataSource)
        ? (metadataSource as PostgresSource).configuration?.connection_set
        : undefined,
    });

    if (isEditMode) {
      editConnection({ originalName: dataSourceName, ...payload });
    } else {
      createConnection(payload);
    }
  };

  const {
    Form,
    methods: { reset },
  } = useConsoleForm({
    schema,
  });

  useEffect(() => {
    try {
      reset(getDefaultValues(metadataSource));
    } catch (err) {
      hasuraToast({
        type: 'error',
        title:
          'Error while retriving database. Please check if the database is of type postgres',
      });
    }
  }, [metadataSource, reset]);

  const hiddenOptions =
    overrideDriver === 'cockroach' ? ['connectionParams'] : [];

  const dynamicDBRoutingTab =
    dataSourceName && isEditMode && window.__env.consoleType !== 'oss'
      ? [
          {
            value: 'dynamicDBRouting',
            label: 'Dynamic Routing',
            content: (
              <div className="mt-2">
                <LimitedFeatureWrapper
                  id="dynamic-db-routing"
                  title="Dynamic Routing for Databases"
                  description="Effortlessly scale your data architecture with Dynamic Routing for databases, allowing you to easily route GraphQL requests to different database connections and leverage different database topology patterns with Hasura."
                >
                  <DynamicDBRouting sourceName={dataSourceName} />
                </LimitedFeatureWrapper>
              </div>
            ),
          },
        ]
      : [];

  return (
    <>
      <Heading size="5">
        {isEditMode
          ? `Edit ${overrideDisplayName ?? 'Postgres'} Connection`
          : `Connect ${overrideDisplayName ?? 'Postgres'} Database`}
      </Heading>
      <div className="my-4" />
      <Tabs
        value={tab}
        onValueChange={(value) => setTab(value)}
        items={[
          {
            value: 'connectionDetails',
            label: 'Connection Details',
            content: (
              <div className="mt-2">
                <Form onSubmit={handleSubmit}>
                  <ConnectPostgresForm hiddenOptions={hiddenOptions} />
                  <Flex justify="end" className="mt-2">
                    <Button
                      type="submit"
                      mode="primary"
                      loading={isPending}
                      loadingText="Saving"
                    >
                      {isEditMode ? 'Update Connection' : 'Connect Database'}
                    </Button>
                  </Flex>
                </Form>
              </div>
            ),
          },
          ...dynamicDBRoutingTab,
        ]}
      />
    </>
  );
};
