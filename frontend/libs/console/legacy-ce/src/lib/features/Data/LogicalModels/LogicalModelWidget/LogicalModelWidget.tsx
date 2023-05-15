import Skeleton from 'react-loading-skeleton';
import { CreateBooleanMap } from '../../../../components/Common/utils/tsUtils';
import { Driver, drivers } from '../../../../dataSources';
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
import { LogicalModelFormInputs } from './parts/LogicalModelFormInputs';
import {
  AddLogicalModelFormData,
  addLogicalModelValidationSchema,
} from './validationSchema';

export type AddLogicalModelDialogProps = {
  defaultValues?: AddLogicalModelFormData;
  onCancel?: () => void;
  onSubmit?: () => void;
  disabled?: CreateBooleanMap<AddLogicalModelFormData>;
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

  /**
   * Options for the data sources
   */
  const {
    data: sourceOptions = [],
    error: sourceOptionError,
    isLoading: isMetadataLoading,
  } = useMetadata(m =>
    m.metadata.sources
      .filter(s => drivers.includes(s.kind as Driver))
      .map(source => ({
        value: source.name,
        label: source.name,
      }))
  );

  /**
   * Options for the data source types
   */
  const {
    data: typeOptions = [],
    error: typeOptionError,
    isLoading: isIntrospectionLoading,
  } = useSupportedDataTypes({
    dataSourceName: watch('dataSourceName'),
    select: values => {
      if (values === Feature.NotImplemented) return [];
      return Object.values(values).flat();
    },
    options: {
      enabled: !!watch('dataSourceName'),
    },
  });

  const onSubmit = (data: AddLogicalModelFormData) => {
    trackLogicalModel({
      data,
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
        <div>{sourceOptionError}</div>
        <div> {typeOptionError?.message}</div>
      </IndicatorCard>
    );

  if (!props.asDialog)
    return isMetadataLoading || isIntrospectionLoading ? (
      <Skeleton count={8} height={20} />
    ) : (
      <Form onSubmit={onSubmit}>
        <LogicalModelFormInputs
          sourceOptions={sourceOptions}
          typeOptions={typeOptions}
          disabled={props.disabled}
        />

        <div className="flex justify-end">
          <Button type="submit" mode="primary" isLoading={isLoading}>
            {isEditMode ? 'Edit Logical Model' : 'Create Logical Model'}
          </Button>
        </div>
      </Form>
    );

  return (
    <Dialog
      size="xl"
      footer={
        <Dialog.Footer
          onSubmit={() => {
            handleSubmit(onSubmit)();
          }}
          onClose={props.onCancel}
          isLoading={isLoading}
          callToDeny="Cancel"
          callToAction="Create Logical Model"
          onSubmitAnalyticsName="actions-tab-generate-types-submit"
          onCancelAnalyticsName="actions-tab-generate-types-cancel"
        />
      }
      title="Add Logical Model"
      hasBackdrop
      onClose={props.onCancel}
    >
      <div className="px-md">
        {isMetadataLoading || isIntrospectionLoading ? (
          <Skeleton count={8} height={20} />
        ) : (
          <>
            <p className="text-muted mb-6">
              Creating a logical model in advance can help generate Native
              Queries faster
            </p>
            <Form onSubmit={() => {}}>
              <LogicalModelFormInputs
                sourceOptions={sourceOptions}
                typeOptions={typeOptions}
                disabled={props.disabled}
              />
            </Form>
          </>
        )}
      </div>
    </Dialog>
  );
};
