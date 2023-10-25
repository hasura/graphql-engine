import {
  InputField,
  Select,
  useConsoleForm,
} from '../../../../new-components/Form';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import { Feature } from '../../../DataSource';
import { getTableDisplayName } from '../../../DatabaseRelationships';
import { useSupportedDataTypes } from '../../hooks/useSupportedDataTypes';
import { Collapsible } from '../../../../new-components/Collapsible';
import { Button } from '../../../../new-components/Button';
import Skeleton from 'react-loading-skeleton';
import { useTrackStoredProcedure } from '../../hooks/useTrackStoredProcedure';
import { StoredProcedureArgument } from '../../../hasura-metadata-types';
import {
  Routes,
  STORED_PROCEDURE_TRACK_ERROR,
  STORED_PROCEDURE_TRACK_SUCCESS,
} from '../constants';
import { hasuraToast } from '../../../../new-components/Toasts';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';
import { ArgumentsInput } from './components/ArgumentsInput';
import { cleanEmpty } from '../../../ConnectDBRedesign/components/ConnectPostgresWidget/utils/helpers';
import { useStoredProcedures } from '../../hooks/useStoredProcedures';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { APIError } from '../../../../hooks/error';
import {
  AddStoredProcedureFormData,
  defaultEmptyValues,
  trackStoredProcedureValidationSchema,
} from './schema';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';
import { useState } from 'react';
import { usePushRoute } from '../../../ConnectDBRedesign/hooks';
import { BiRefresh } from 'react-icons/bi';

export const StoredProcedureWidget = () => {
  const {
    Form,
    methods: { watch },
  } = useConsoleForm({
    schema: trackStoredProcedureValidationSchema,
    options: {
      defaultValues: defaultEmptyValues,
    },
  });

  const { trackStoredProcedure, isLoading } = useTrackStoredProcedure();
  const [isLogicalModelWidgetOpen, setIsLogicalModelWidgetOpen] =
    useState(false);

  const dataSourceName = watch('dataSourceName');

  const {
    data: { sourceOptions = [], logicalModelOptions = [] } = {},
    isLoading: isMetadataLoading,
    error: metadataError,
  } = useMetadata(m => ({
    sourceOptions: MetadataSelectors.getSources()(m)
      .filter(source => source.kind === 'mssql') // we need a better hook to supported driver by feature
      .map(source => ({
        label: source.name,
        value: source.name,
      })),
    logicalModelOptions: MetadataSelectors.findSource(dataSourceName)(
      m
    )?.logical_models?.map(logicalModel => ({
      label: logicalModel.name,
      value: logicalModel.name,
    })),
  }));

  const {
    data: storedProcedureOptions = [],
    isLoading: isIntrospectionLoading,
    error: introspectionError,
    refetch,
  } = useStoredProcedures({
    dataSourceName,
    select: data => {
      if (data === Feature.NotImplemented) return [];

      return data.map(sp => ({
        label: getTableDisplayName(sp),
        value: JSON.stringify(sp),
      }));
    },
    options: {
      // don't fire until there is a dataSource
      enabled: !!dataSourceName,
    },
  });

  /**
   * Options for the data source types
   */
  const {
    data: typeOptions = [],
    isLoading: areTypeOptionsLoading,
    error: typeIntrospectionError,
  } = useSupportedDataTypes({
    dataSourceName,
    options: {
      enabled: !!dataSourceName,
    },
  });

  const pushRoute = usePushRoute();

  if (isMetadataLoading)
    return <Skeleton count={8} height={25} className="mb-2" />;

  if (metadataError || typeIntrospectionError || introspectionError) {
    return (
      <IndicatorCard status="negative" headline="Error" id="error-card">
        {[
          (metadataError as APIError)?.message,
          typeIntrospectionError?.message,
          introspectionError?.message,
        ].map(error => error && <div>{error}</div>)}
      </IndicatorCard>
    );
  }

  const handleSubmit = (formData: AddStoredProcedureFormData) => {
    const payload = {
      ...formData,
      stored_procedure: JSON.parse(formData.stored_procedure),
      arguments: formData.arguments?.reduce(
        (acc, { name, ...restOfTheProperties }) => ({
          ...acc,
          [name]: {
            ...restOfTheProperties,
          },
        }),
        {} as Record<string, StoredProcedureArgument>
      ),
    };
    trackStoredProcedure({
      data: cleanEmpty(payload),
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: STORED_PROCEDURE_TRACK_SUCCESS,
        });
        pushRoute(Routes.StoredProcedures);
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: STORED_PROCEDURE_TRACK_ERROR,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });
  };

  return (
    <Form onSubmit={handleSubmit}>
      <Select
        name="dataSourceName"
        label="Select a source"
        options={sourceOptions}
        placeholder="Select a source"
      />

      {isIntrospectionLoading ? (
        <Skeleton count={4} height={25} className="mb-2" />
      ) : (
        <>
          <div className="flex items-center gap-2">
            <Select
              name="stored_procedure"
              label="Select a stored procedure"
              placeholder="Stored Procedure"
              options={storedProcedureOptions}
              disabled={!storedProcedureOptions.length && dataSourceName}
            />
            <div className="mt-3">
              <Button
                icon={<BiRefresh />}
                onClick={() => refetch()}
                disabled={!dataSourceName}
              />
            </div>
          </div>

          {!storedProcedureOptions.length && dataSourceName ? (
            <IndicatorCard headline="No Stored Procedures Found" status="info">
              There are no stored prodecures found for the selected data source
              <code className="bg-slate-100 rounded text-red-600 ml-1.5">
                {dataSourceName}
              </code>
            </IndicatorCard>
          ) : null}
        </>
      )}

      <Collapsible
        triggerChildren={
          <div className="font-semibold text-muted">Advanced</div>
        }
      >
        <Select
          name="configuration.exposed_as"
          label="Expose the procedure as"
          disabled
          options={[{ label: 'query', value: 'query' }]}
        />
        <InputField
          name="configuration.custom_name"
          label="Custom Name"
          placeholder="If omitted, will use the stored_procedure name"
        />
      </Collapsible>
      <hr className="my-md" />

      {areTypeOptionsLoading ? (
        <Skeleton count={4} height={25} className="mb-2" />
      ) : (
        <ArgumentsInput name="arguments" types={typeOptions} />
      )}

      <Select
        name="returns"
        label="Return Type"
        placeholder="Select a return type"
        options={logicalModelOptions}
        disabled={!logicalModelOptions.length && dataSourceName}
      />

      {!logicalModelOptions.length && dataSourceName ? (
        <IndicatorCard headline="No Logical Models Found" status="info">
          <div>
            Looks like you do not have any Logical Models associated with
            <code className="bg-slate-100 rounded text-red-600 ml-1.5">
              {dataSourceName}
            </code>
            . Tracking Stored Procedure in Hasura requires a Logical Model to be
            used as the return type. You can create one on the fly.
          </div>

          <div className="mt-sm">
            <Button
              onClick={() => {
                setIsLogicalModelWidgetOpen(true);
              }}
            >
              Create Logical Model
            </Button>
          </div>
        </IndicatorCard>
      ) : (
        <div className="flex justify-end">
          <Button
            onClick={() => {
              setIsLogicalModelWidgetOpen(true);
            }}
          >
            Create Logical Model
          </Button>
        </div>
      )}

      <hr className="my-md" />

      {isLogicalModelWidgetOpen ? (
        <LogicalModelWidget
          asDialog
          onSubmit={() => {
            setIsLogicalModelWidgetOpen(false);
          }}
          onCancel={() => {
            setIsLogicalModelWidgetOpen(false);
          }}
        />
      ) : null}

      <div className="flex justify-end">
        <Button type="submit" mode="primary" isLoading={isLoading}>
          Track Stored Procedure
        </Button>
      </div>
    </Form>
  );
};
