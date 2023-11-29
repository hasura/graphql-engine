import { useConsoleForm } from '../../../../new-components/Form';
import { getDefaultValues, PostgresConnectionSchema, schema } from './schema';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useMetadata } from '../../../hasura-metadata-api';
import { generatePostgresRequestPayload } from './utils/generateRequests';
import { useEffect, useState } from 'react';
import { LimitedFeatureWrapper } from '../LimitedFeatureWrapper/LimitedFeatureWrapper';
import { DynamicDBRouting } from './parts/DynamicDBRouting';
import { Tabs } from '../../../../new-components/Tabs';
import { DisplayToastErrorMessage } from '../Common/DisplayToastErrorMessage';
import { ConnectPostgresForm } from './parts/ConnectPostgresForm';
import { Button } from '../../../../new-components/Button';

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

  const { data: metadataSource } = useMetadata(m =>
    m.metadata.sources.find(source => source.name === dataSourceName)
  );

  const { createConnection, editConnection, isLoading } =
    useManageDatabaseConnection({
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: isEditMode
            ? 'Database updated successfully!'
            : 'Database added successfully!',
        });
      },
      onError: err => {
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
              <div className="mt-sm">
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
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode
          ? `Edit ${overrideDisplayName ?? 'Postgres'} Connection`
          : `Connect ${overrideDisplayName ?? 'Postgres'} Database`}
      </div>
      <div className="my-3" />
      <Tabs
        value={tab}
        onValueChange={value => setTab(value)}
        items={[
          {
            value: 'connectionDetails',
            label: 'Connection Details',
            content: (
              <div className="mt-sm">
                <Form onSubmit={handleSubmit}>
                  <ConnectPostgresForm hiddenOptions={hiddenOptions} />
                  <div className="flex justify-end mt-sm">
                    <Button
                      type="submit"
                      mode="primary"
                      isLoading={isLoading}
                      loadingText="Saving"
                    >
                      {isEditMode ? 'Update Connection' : 'Connect Database'}
                    </Button>
                  </div>
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
