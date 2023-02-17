import { DataSource, Feature } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { OpenApi3Form } from '@/features/OpenApi3Form';
import { Button } from '@/new-components/Button';
import { transformSchemaToZodObject } from '@/features/OpenApi3Form/utils';
import { InputField, useConsoleForm } from '@/new-components/Form';
import { Tabs } from '@/new-components/Tabs';
import { get } from 'lodash';
import { useEffect, useState } from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';
import { useQuery } from 'react-query';
import { z, ZodSchema } from 'zod';
import { graphQLCustomizationSchema } from '../GraphQLCustomization/schema';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { useMetadata } from '@/features/hasura-metadata-api';
import { adaptGraphQLCustomization } from '../GraphQLCustomization/utils/adaptResponse';
import { generateGDCRequestPayload } from './utils/generateRequest';
import { hasuraToast } from '@/new-components/Toasts';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { capitaliseFirstLetter } from '@/components/Common/ConfigureTransformation/utils';

interface ConnectGDCSourceWidgetProps {
  driver: string;
  dataSourceName?: string;
}

const useFormValidationSchema = (driver: string) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['form-schema', driver],
    queryFn: async () => {
      const configSchemas = await DataSource(
        httpClient
      ).connectDB.getConfigSchema(driver);

      if (!configSchemas || configSchemas === Feature.NotImplemented)
        throw Error('Could not retrive config schema info for driver');

      const validationSchema = z.object({
        name: z.string().min(1, 'Name is a required field!'),
        configuration: transformSchemaToZodObject(
          configSchemas.configSchema,
          configSchemas.otherSchemas
        ),
        customization: graphQLCustomizationSchema.optional(),
      });

      return { validationSchema, configSchemas };
    },
    refetchOnWindowFocus: false,
  });
};

export const ConnectGDCSourceWidget = (props: ConnectGDCSourceWidgetProps) => {
  const { driver, dataSourceName } = props;
  const [tab, setTab] = useState('connection_details');

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
          title: 'An error occurred while adding database',
          children: JSON.stringify(err),
        });
      },
    });

  const isEditMode = !!dataSourceName;

  const { data } = useFormValidationSchema(driver);

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
        customization: adaptGraphQLCustomization(
          metadataSource?.customization ?? {}
        ),
      });
  }, [metadataSource, reset]);

  if (!data?.configSchemas) return null;

  const handleSubmit = (formValues: any) => {
    const payload = generateGDCRequestPayload({
      driver,
      values: formValues,
    });

    if (isEditMode) {
      editConnection(payload);
    } else {
      createConnection(payload);
    }
  };

  const connectionDetailsTabErrors = [
    get(formState.errors, 'name'),
    get(formState.errors, 'configuration.connectionInfo'),
  ].filter(Boolean);

  console.log(formState.errors);

  return (
    <div>
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode
          ? `Edit ${capitaliseFirstLetter(driver)} Connection`
          : `Connect New ${capitaliseFirstLetter(driver)} Database`}
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
                </div>
              ),
            },
            {
              value: 'customization',
              label: 'GraphQL Customization',
              content: <GraphQLCustomization name="customization" />,
            },
          ]}
        />
        <div className="flex justify-end">
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
  );
};
