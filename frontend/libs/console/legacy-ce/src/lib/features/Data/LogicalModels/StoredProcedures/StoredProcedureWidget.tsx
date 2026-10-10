import {
  InputField,
  useConsoleForm,
  Collapsible,
  Button,
  hasuraToast,
  IndicatorCard,
  DisplayToastErrorMessage,
  SkeletonList,
  SelectField,
} from '@hasura/shared/ui';
import { useMetadata } from '@hasura/metadata/api';
import {
  useStoredProcedures,
  useSupportedScalars,
} from '@hasura/metadata/data-source';
import { useTrackStoredProcedure } from '../../hooks/useTrackStoredProcedure';
import { StoredProcedureArgument } from '@hasura/shared/types';
import {
  Routes,
  STORED_PROCEDURE_TRACK_ERROR,
  STORED_PROCEDURE_TRACK_SUCCESS,
} from '../constants';
import { ArgumentsInput } from './components/ArgumentsInput';
import { cleanEmpty } from '../../../ConnectDBRedesign/components/ConnectPostgresWidget/utils/helpers';
import {
  AddStoredProcedureFormData,
  defaultEmptyValues,
  trackStoredProcedureValidationSchema,
} from './schema';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';
import { useState } from 'react';
import { BiRefresh } from 'react-icons/bi';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { getTableDisplayName } from '@hasura/shared/utils';
import { Flex } from '@radix-ui/themes';
import { useNavigate } from 'react-router';

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

  const { trackStoredProcedure, isPending } = useTrackStoredProcedure();
  const [isLogicalModelWidgetOpen, setIsLogicalModelWidgetOpen] =
    useState(false);

  const dataSourceName = watch('dataSourceName');

  const {
    data: meta,
    isLoading: isMetadataLoading,
    error: metadataError,
  } = useMetadata();

  const source = MetadataSelectors.findSource(dataSourceName)(meta);

  const {
    data: storedProcedureOptions = [],
    isLoading: isIntrospectionLoading,
    error: introspectionError,
    refetch,
  } = useStoredProcedures(
    {
      source: source!,
    },
    {
      select: (data) => {
        return data.map((sp) => ({
          label: getTableDisplayName(sp.stored_procedure),
          value: JSON.stringify(sp),
        }));
      },
      // don't fire until there is a dataSource
      enabled: !!source,
    },
  );

  /**
   * Options for the data source types
   */
  const {
    data: typeOptions,
    isLoading: areTypeOptionsLoading,
    error: typeIntrospectionError,
  } = useSupportedScalars(source?.kind, {
    enabled: !!dataSourceName,
  });

  const pushRoute = useNavigate();

  if (isMetadataLoading)
    return <SkeletonList count={5} containerClassName="mb-2" />;

  if (metadataError || typeIntrospectionError || introspectionError) {
    return (
      <IndicatorCard status="negative" headline="Error" id="error-card">
        {[
          (metadataError as Error)?.message,
          typeIntrospectionError?.message,
          (introspectionError as Error)?.message,
        ].map((error, index) => error && <div key={index}>{error}</div>)}
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
        {} as Record<string, StoredProcedureArgument>,
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
      onError: (err) => {
        hasuraToast({
          type: 'error',
          title: STORED_PROCEDURE_TRACK_ERROR,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });
  };

  const hasDataSourceName = Boolean(dataSourceName);
  const sourceOptions = MetadataSelectors.getSources()(meta)
    .filter((source) => source.kind === 'mssql') // we need a better hook to supported driver by feature
    .map((source) => ({
      label: source.name,
      value: source.name,
    }));
  const logicalModelOptions = MetadataSelectors.findSource(dataSourceName)(
    meta,
  )?.logical_models?.map((logicalModel) => ({
    label: logicalModel.name,
    value: logicalModel.name,
  }));

  return (
    <Form onSubmit={handleSubmit}>
      <SelectField
        name="dataSourceName"
        label="Select a source"
        options={sourceOptions}
        placeholder="Select a source"
      />

      {isIntrospectionLoading ? (
        <SkeletonList count={4} containerClassName="mb-2" />
      ) : (
        <>
          <Flex align="center" gap="2">
            <SelectField
              name="stored_procedure"
              label="Select a stored procedure"
              placeholder="Stored Procedure"
              options={storedProcedureOptions}
              disabled={!storedProcedureOptions.length && hasDataSourceName}
            />
            <div className="mt-3">
              <Button
                leftIcon={BiRefresh}
                onClick={() => refetch()}
                disabled={!dataSourceName}
              />
            </div>
          </Flex>

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
        <SelectField
          name="configuration.exposed_as"
          label="Expose the procedure as"
          disabled
          options={[{ label: 'query', value: 'query' }]}
        />
        <InputField
          name="configuration.custom_name"
          label="Custom Name"
          fieldProps={{
            placeholder: 'If omitted, will use the stored_procedure name',
          }}
        />
      </Collapsible>
      <hr className="my-4" />

      {areTypeOptionsLoading ? (
        <SkeletonList count={4} containerClassName="mb-2" />
      ) : (
        <ArgumentsInput name="arguments" types={typeOptions ?? []} />
      )}

      <SelectField
        name="returns"
        label="Return Type"
        placeholder="Select a return type"
        options={logicalModelOptions ?? []}
        disabled={!logicalModelOptions?.length && hasDataSourceName}
      />

      {!logicalModelOptions?.length && dataSourceName ? (
        <IndicatorCard headline="No Logical Models Found" status="info">
          <div>
            Looks like you do not have any Logical Models associated with
            <code className="bg-slate-100 rounded text-red-600 ml-1.5">
              {dataSourceName}
            </code>
            . Tracking Stored Procedure in Hasura requires a Logical Model to be
            used as the return type. You can create one on the fly.
          </div>

          <div className="mt-2">
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
        <Flex justify="end">
          <Button
            onClick={() => {
              setIsLogicalModelWidgetOpen(true);
            }}
          >
            Create Logical Model
          </Button>
        </Flex>
      )}

      <hr className="my-4" />

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

      <Flex justify="end">
        <Button type="submit" mode="primary" loading={isPending}>
          Track Stored Procedure
        </Button>
      </Flex>
    </Form>
  );
};
