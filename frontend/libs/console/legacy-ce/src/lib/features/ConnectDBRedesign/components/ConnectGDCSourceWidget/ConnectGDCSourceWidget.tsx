import get from 'lodash/get';
import { useEffect, useState } from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';
import { ZodType, z } from 'zod';
import {
  Button,
  Collapsible,
  InputField,
  useConsoleForm,
  hasuraToast,
  IndicatorCard,
  Tabs,
  OpenApi3Form,
  DisplayToastErrorMessage,
  SkeletonList,
} from '@hasura/shared/ui';
import { useAvailableDrivers } from '@hasura/metadata/data-source';
import { useMetadata } from '@hasura/metadata/api';
import { Source } from '@hasura/shared/types';
import { useManageDatabaseConnection } from '../../hooks/useManageDatabaseConnection';
import { cleanEmpty } from '../ConnectPostgresWidget/utils/helpers';
import { GraphQLCustomization } from '../GraphQLCustomization/GraphQLCustomization';
import { adaptGraphQLCustomization } from '../GraphQLCustomization/utils/adaptResponse';
import { Template } from './components/Template';
import { TemplateVariables } from './components/TemplateVariables';
import { Timeout } from './components/Timeout';
import {
  TemplateVariableMap,
  useFormValidationSchema,
} from './useFormValidationSchema';
import { generateGDCRequestPayload } from './utils/generateRequest';
import { getErrorMessage } from '@hasura/shared/utils';
import { Flex } from '@radix-ui/themes';

interface ConnectGDCSourceWidgetProps {
  driver: string;
  dataSourceName?: string;
}

function getExistingConnectionDetailsFromMetadata(source: Source) {
  const configuration = source.configuration ?? ({} as any);
  const customization = source.customization ?? {};

  const templateVariableMap = (configuration.template_variables ||
    {}) as TemplateVariableMap;

  const templateVariableArray = Object.entries(templateVariableMap).map(
    ([key, values]) => {
      return { name: key, ...values };
    },
  );
  return {
    name: source.name,
    // This is a particularly weird case with metadata only valid for GDC sources.
    configuration: configuration.value,
    timeout: configuration.timeout?.seconds as number | undefined,
    template: (configuration.template ?? '') as string,
    template_variables: templateVariableArray,
    customization: adaptGraphQLCustomization(customization),
  };
}

function hasAdvancedSettings(source: Source | undefined) {
  if (!source) return false;

  const details = cleanEmpty(getExistingConnectionDetailsFromMetadata(source));

  return (
    !!details.timeout || !!details.template || !!details.template_variables
  );
}

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
    drivers?.find((d) => d.name === driver)?.displayName ?? driver;

  const {
    data: metadataSource,
    isLoading: isLoadingMetadata,
    isError: isMetadataError,
    error: metadataError,
  } = useMetadata((m) =>
    m.metadata.sources.find((source) => source.name === dataSourceName),
  );
  const isEditMode = !!dataSourceName;
  const {
    createConnection,
    editConnection,
    isPending: isLoadingCreateConnection,
  } = useManageDatabaseConnection({
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

  const [schema, setSchema] = useState<
    ZodType<Record<string, unknown>, Record<string, unknown>>
  >(z.any());

  const {
    Form,
    methods: { formState, reset },
  } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        template_variables: [],
      },
    },
  });

  useEffect(() => {
    if (data?.validationSchema) setSchema(data.validationSchema);
  }, [data?.validationSchema]);

  useEffect(() => {
    if (metadataSource) {
      reset(getExistingConnectionDetailsFromMetadata(metadataSource));
    }
  }, [metadataSource, reset]);

  if (isLoading) {
    return (
      <div>
        <SkeletonList count={50} />
      </div>
    );
  }

  if (validationSchemaError) {
    const errorMsg = getErrorMessage(
      validationSchemaError,
      'An error occurred loading the connection configuration.',
    );
    return <IndicatorCard status="negative">{errorMsg}</IndicatorCard>;
  }
  if (metadataError) {
    const errorMsg = getErrorMessage(
      metadataError,
      'An error occurred loading metadata.',
    );

    return <IndicatorCard status="negative">{errorMsg}</IndicatorCard>;
  }
  if (availableDriversError) {
    const errorMsg = getErrorMessage(
      availableDriversError,
      'An error occurred loading the available drivers.',
    );
    return <IndicatorCard status="negative">{errorMsg}</IndicatorCard>;
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

  const openAdvanced = isEditMode && hasAdvancedSettings(metadataSource);

  return (
    <div>
      <div className="text-xl text-gray-600 font-semibold">
        {isEditMode
          ? `Edit ${driverDisplayName} Connection`
          : `Connect ${driverDisplayName} Database`}
      </div>
      <div className="my-3" />
      <Form onSubmit={handleSubmit}>
        <Tabs
          value={tab}
          onValueChange={(value) => setTab(value)}
          items={[
            {
              value: 'connection_details',
              label: 'Connection Details',
              icon: connectionDetailsTabErrors.length ? (
                <FaExclamationTriangle className="text-red-800" />
              ) : undefined,
              content: (
                <div className="mt-2">
                  <InputField
                    name="name"
                    label="Database Name"
                    fieldProps={{
                      placeholder: 'Database name',
                    }}
                  />
                  <OpenApi3Form
                    name="configuration"
                    schemaObject={data?.configSchemas.configSchema}
                    references={data?.configSchemas.otherSchemas}
                  />

                  <div className="mt-2">
                    <Collapsible
                      defaultOpen={openAdvanced}
                      triggerChildren={
                        <div className="font-semibold text-muted">
                          Advanced Settings
                        </div>
                      }
                    >
                      <Timeout name="timeout" />
                      <Template name="template" />
                      <TemplateVariables />
                    </Collapsible>
                  </div>

                  <div className="mt-2">
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
        <Flex justify="end">
          <Button
            type="submit"
            mode="primary"
            loading={isLoadingCreateConnection}
            loadingText="Saving"
          >
            {isEditMode ? 'Update Connection' : 'Connect Database'}
          </Button>
        </Flex>
      </Form>
    </div>
  );
};
