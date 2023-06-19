import Skeleton from 'react-loading-skeleton';
import { CreateBooleanMap } from '../../../../components/Common/utils/tsUtils';
import { Button } from '../../../../new-components/Button';
import { Dialog } from '../../../../new-components/Dialog';
import { useConsoleForm } from '../../../../new-components/Form';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { hasuraToast } from '../../../../new-components/Toasts';
import { Feature } from '../../../DataSource';
import { useMetadata } from '../../../hasura-metadata-api';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';
import { useSupportedDataTypes } from '../../hooks/useSupportedDataTypes';
import { useTrackLogicalModel } from '../../hooks/useTrackLogicalModel';
import {
  LOGICAL_MODEL_CREATE_ERROR,
  LOGICAL_MODEL_CREATE_SUCCESS,
} from '../constants';
import { LogicalModelFormInputs } from './components/LogicalModelFormInputs';
import {
  AddLogicalModelFormData,
  addLogicalModelValidationSchema,
} from './validationSchema';
import { extractModelsAndQueriesFromMetadata } from '../../../hasura-metadata-api/selectors';
import { Link } from 'react-router';
import { formFieldToLogicalModelField } from './mocks/utils/formFieldToLogicalModelField';
import { useSupportedDrivesForNativeQueries } from '../hook';

export type AddLogicalModelDialogProps = {
  defaultValues?: AddLogicalModelFormData;
  onCancel?: () => void;
  onSubmit?: () => void;
  disabled?: CreateBooleanMap<
    AddLogicalModelFormData & {
      callToAction?: boolean;
    }
  >;
  asDialog?: boolean;
};

export const LogicalModelWidget = (props: AddLogicalModelDialogProps) => {
  const { trackLogicalModel, isLoading } = useTrackLogicalModel();

  /**
   * If the name is already present, then the form is in edit mode.
   */
  const isEditMode = !!props.defaultValues?.name;

  const {
    Form,
    methods: { watch, handleSubmit },
  } = useConsoleForm({
    schema: addLogicalModelValidationSchema,
    options: {
      defaultValues: props.defaultValues,
    },
  });
  const selectedDataSource = watch('dataSourceName');
  const allowedDrivers = useSupportedDrivesForNativeQueries();

  /**
   * Options for the data sources
   */
  const {
    data: sourceOptions = [],
    error: sourceOptionError,
    isLoading: isMetadataLoading,
  } = useMetadata(m =>
    m.metadata.sources
      .filter(s => allowedDrivers.includes(s.kind))
      .map(source => ({
        value: source.name,
        label: source.name,
      }))
  );

  const { data: isThereBigQueryOrMssqlSource } = useMetadata(
    m =>
      !!m.metadata.sources.find(
        s => s.kind === 'mssql' || s.kind === 'bigquery'
      )
  );

  /**
   * Options for the data source types
   */
  const {
    data: typeOptions = [],
    error: typeOptionError,
    isLoading: isIntrospectionLoading,
  } = useSupportedDataTypes({
    dataSourceName: selectedDataSource,
    select: values => {
      if (values === Feature.NotImplemented) return [];
      return Object.values(values).flat();
    },
    options: {
      enabled: !!selectedDataSource,
    },
  });

  const { data: modelsAndQueries } = useMetadata(m =>
    extractModelsAndQueriesFromMetadata(m)
  );

  const logicalModels = modelsAndQueries?.models || [];

  const onSubmit = (data: AddLogicalModelFormData) => {
    trackLogicalModel({
      data: {
        dataSourceName: data.dataSourceName,
        name: data.name,
        fields: data.fields.map(formFieldToLogicalModelField),
      },
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: LOGICAL_MODEL_CREATE_SUCCESS,
        });
        props.onSubmit?.();
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: LOGICAL_MODEL_CREATE_ERROR,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });
  };

  if (sourceOptionError || typeOptionError)
    return (
      <IndicatorCard status="negative" headline="Internal Error">
        <div>{sourceOptionError?.toString()}</div>
        <div> {typeOptionError?.message}</div>
      </IndicatorCard>
    );

  if (!props.asDialog)
    return isMetadataLoading || isIntrospectionLoading ? (
      <Skeleton count={8} height={20} />
    ) : (
      <>
        {isEditMode && (
          <IndicatorCard status="info">
            The current release does not support editing Logical Models. This
            feature will be available in a future release. You can still{' '}
            <Link
              to={`/data/native-queries/logical-models/${props.defaultValues?.dataSourceName}/${props.defaultValues?.name}/permissions`}
            >
              edit permissions
            </Link>{' '}
            or edit logical models directly by modifying the Metadata.
          </IndicatorCard>
        )}
        <Form onSubmit={onSubmit}>
          <LogicalModelFormInputs
            sourceOptions={sourceOptions}
            typeOptions={typeOptions}
            disabled={props.disabled}
            logicalModels={logicalModels}
          />
          <div className="flex justify-end">
            <Button
              disabled={props.disabled?.callToAction}
              type="submit"
              mode="primary"
              isLoading={isLoading}
            >
              {isEditMode ? 'Edit Logical Model' : 'Create Logical Model'}
            </Button>
          </div>
        </Form>
      </>
    );

  return (
    <Dialog
      size="xl"
      description="Creating a logical model in advance can help generate Native Queries faster"
      footer={{
        onSubmit: () => handleSubmit(onSubmit)(),
        onClose: props.onCancel,
        isLoading,
        callToDeny: 'Cancel',
        callToAction: 'Create Logical Model',
        onSubmitAnalyticsName: 'actions-tab-generate-types-submit',
        onCancelAnalyticsName: 'actions-tab-generate-types-cancel',
      }}
      title="Add Logical Model"
      hasBackdrop
      onClose={props.onCancel}
    >
      <div className="px-md">
        {isMetadataLoading || isIntrospectionLoading ? (
          <Skeleton count={8} height={20} />
        ) : (
          <Form
            onSubmit={() => {
              // this is handled in the footer of the dialog
            }}
          >
            <LogicalModelFormInputs
              sourceOptions={sourceOptions}
              typeOptions={typeOptions}
              disabled={props.disabled}
              logicalModels={logicalModels}
              isThereBigQueryOrMssqlSource={isThereBigQueryOrMssqlSource}
            />
          </Form>
        )}
      </div>
    </Dialog>
  );
};
