import to from 'await-to-js';
import { AxiosError } from 'axios';
import get from 'lodash/get';
import { useEffect, useState } from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import { useQuery } from 'react-query';
import { ZodSchema, z } from 'zod';
import { Button } from '../../../../new-components/Button';
import { Collapsible } from '../../../../new-components/Collapsible';
import { InputField, useConsoleForm } from '../../../../new-components/Form';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { Tabs } from '../../../../new-components/Tabs';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useAvailableDrivers } from '../../../ConnectDB/hooks';
import { DataSource, Feature } from '../../../DataSource';
import { useHttpClient } from '../../../Network';
import { OpenApi3Form } from '../../../OpenApi3Form';
import { transformSchemaToZodObject } from '../../../OpenApi3Form/utils';
import { useMetadata } from '../../../hasura-metadata-api';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { DisplayToastErrorMessage } from '../Common/DisplayToastErrorMessage';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { graphQLCustomizationSchema } from '../GraphQLCustomization/schema';
import { adaptGraphQLCustomization } from '../GraphQLCustomization/utils/adaptResponse';
import { generateGDCRequestPayload } from './utils/generateRequest';
import { Timeout } from './components/Timeout';
import { Template } from './components/Template';

interface ConnectGDCSourceWidgetProps {
  driver: string;
  dataSourceName?: string;
}

const useFormValidationSchema = (driver: string) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['form-schema', driver],
    queryFn: async () => {
      const [err, configSchemas] = await to(
        DataSource(httpClient).connectDB.getConfigSchema(driver)
      );

      if (err) {
        throw err;
      }

      if (!configSchemas || configSchemas === Feature.NotImplemented)
        throw Error('Could not retrive config schema info for driver');

      const validationSchema = z.object({
        name: z.string().min(1, 'Name is a required field!'),
        configuration: transformSchemaToZodObject(
          configSchemas.configSchema,
          configSchemas.otherSchemas
        ),
        customization: graphQLCustomizationSchema.optional(),
        timeout: z
          .number()
          .gte(0, { message: 'Timeout must be a postive number' })
          .optional(),
        template: z.string().optional(),
      });

      return { validationSchema, configSchemas };
    },
    refetchOnWindowFocus: false,
  });
};

export const ConnectGDCSourceWidget = (props: ConnectGDCSourceWidgetProps) => {
  const { driver, dataSourceName } = props;
  const [tab, setTab] = useState('connection_details');

  const {
    data: drivers,
    isLoading: isLoadingAvailableDrivers,
    isError: isAvailableDriversError,
    error: availableDriversError,
  } = useAvailableDrivers();
  const driverDisplayName =
    drivers?.find(d => d.name === driver)?.displayName ?? driver;

  const {
    data: metadataSource,
    isLoading: isLoadingMetadata,
    isError: isMetadataError,
    error: metadataError,
  } = useMetadata(m =>
    m.metadata.sources.find(source => source.name === dataSourceName)
  );
  const isEditMode = !!dataSourceName;
  const {
    createConnection,
    editConnection,
    isLoading: isLoadingCreateConnection,
  } = useManageDatabaseConnection({
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

  const {
    data,
    isLoading: isLoadingValidationSchema,
    isError: isValidationSchemaError,
    error: validationSchemaError,
  } = useFormValidationSchema(driver);

  const isLoading =
    (isLoadingMetadata && !isMetadataError) ||
    (isLoadingValidationSchema && !isValidationSchemaError) ||
    (isLoadingAvailableDrivers && !isAvailableDriversError);

  const [schema, setSchema] = useState<ZodSchema>(z.any());

  const {
    Form,
    methods: { formState, reset },
  } = useConsoleForm({
    schema,
  });

  useEffect(() => {
    if (data?.validationSchema) setSchema(data.validationSchema);
  }, [data?.validationSchema]);

  useEffect(() => {
    if (metadataSource)
      reset({
        name: metadataSource?.name,
        // This is a particularly weird case with metadata only valid for GDC sources.
        configuration: (metadataSource?.configuration as any).value,
        timeout: (metadataSource?.configuration as any)?.timeout?.seconds,
        template: (metadataSource?.configuration as any)?.template ?? '',
        customization: adaptGraphQLCustomization(
          metadataSource?.customization ?? {}
        ),
      });
  }, [metadataSource, reset]);

  if (isLoading) {
    return (
      <div>
        <Skeleton count={10} height={30} />
      </div>
    );
  }

  if (validationSchemaError) {
    const err = validationSchemaError as AxiosError<{ error?: string }>;
    return (
      <IndicatorCard status="negative">
        {err?.response?.data?.error ||
          err.toString() ||
          'An error occurred loading the connection configuration.'}
      </IndicatorCard>
    );
  }
  if (metadataError) {
    const err = metadataError as AxiosError<{ error?: string }>;
    return (
      <IndicatorCard status="negative">
        {err?.response?.data?.error ||
          err.toString() ||
          'An error occurred loading metadata.'}
      </IndicatorCard>
    );
  }
  if (availableDriversError) {
    const err = availableDriversError as AxiosError<{ error?: string }>;
    return (
      <IndicatorCard status="negative">
        {err?.response?.data?.error ||
          err.toString() ||
          'An error occurred loading the available drivers.'}
      </IndicatorCard>
    );
  }

  if (!data?.configSchemas) {
    return (
      <IndicatorCard status="negative">
        An error occurred loading the connection configuration.
      </IndicatorCard>
    );
  }

  const handleSubmit = (formValues: any) => {
    const payload = generateGDCRequestPayload({
      driver,
      values: formValues,
    });

    if (isEditMode) {
      editConnection({ originalName: dataSourceName, ...payload });
    } else {
      createConnection(payload);
    }
  };

  const connectionDetailsTabErrors = [
    get(formState.errors, 'name'),
    get(formState.errors, 'configuration.connectionInfo'),
  ].filter(Boolean);

  return (
    <div>
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode
          ? `Edit ${driverDisplayName} Connection`
          : `Connect ${driverDisplayName} Database`}
      </div>
      <Form onSubmit={handleSubmit}>
        <Tabs
          value={tab}
          onValueChange={value => setTab(value)}
          items={[
            {
              value: 'connection_details',
              label: 'Connection Details',
              icon: connectionDetailsTabErrors.length ? (
                <FaExclamationTriangle className="text-red-800" />
              ) : undefined,
              content: (
                <div className="mt-sm">
                  <InputField
                    name="name"
                    label="Database Name"
                    placeholder="Database name"
                  />
                  <OpenApi3Form
                    name="configuration"
                    schemaObject={data?.configSchemas.configSchema}
                    references={data?.configSchemas.otherSchemas}
                  />

                  <div className="mt-sm">
                    <Collapsible
                      triggerChildren={
                        <div className="font-semibold text-muted">
                          Advanced Settings
                        </div>
                      }
                    >
                      <Timeout name="timeout" />
                      <Template name="template" />
                    </Collapsible>
                  </div>

                  <div className="mt-sm">
                    <Collapsible
                      triggerChildren={
                        <div className="font-semibold text-muted">
                          GraphQL Customization
                        </div>
                      }
                    >
                      <GraphQLCustomization name="customization" />
                    </Collapsible>
                  </div>
                </div>
              ),
            },
          ]}
        />
        <div className="flex justify-end">
          <Button
            type="submit"
            mode="primary"
            isLoading={isLoadingCreateConnection}
            loadingText="Saving"
          >
            {isEditMode ? 'Update Connection' : 'Connect Database'}
          </Button>
        </div>
      </Form>
    </div>
  );
};
