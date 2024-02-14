import { useCallback, useMemo } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaSave } from 'react-icons/fa';
import { CreateBooleanMap } from '../../../../components/Common/utils/tsUtils';
import { Badge } from '../../../../new-components/Badge';
import { Button } from '../../../../new-components/Button';
import { Collapsible } from '../../../../new-components/Collapsible';
import { Dialog } from '../../../../new-components/Dialog';
import { useConsoleForm } from '../../../../new-components/Form';
import { hasuraToast } from '../../../../new-components/Toasts';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import { extractModelsAndQueriesFromMetadata } from '../../../hasura-metadata-api/selectors';
import { Metadata, Source } from '../../../hasura-metadata-types';
import { ReactQueryStatusUI } from '../../components';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';

import { multipleQueryUtils } from '../../components/ReactQueryWrappers/utils';
import { useAllDriverCapabilities } from '../../hooks/useAllDriverCapabilities';
import { useTrackLogicalModel } from '../../hooks/useTrackLogicalModel';
import { DisplayReferencedLogicalModelEntities } from '../LogicalModel/DisplayLogicalModelReferencedEntities';
import { findReferencedEntities } from '../LogicalModel/utils/findReferencedEntities';
import {
  LOGICAL_MODEL_CREATE_ERROR,
  LOGICAL_MODEL_CREATE_SUCCESS,
  LOGICAL_MODEL_EDIT_ERROR,
  LOGICAL_MODEL_EDIT_SUCCESS,
} from '../constants';
import { useSupportedDriversForNativeQueries } from '../hook';
import { LogicalModelWithSource, NativeQueryWithSource } from '../types';
import { LogicalModelFormInputs } from './components/LogicalModelFormInputs';
import { formFieldToLogicalModelField } from './mocks/utils/formFieldToLogicalModelField';
import { supportsSchemaLessTables } from './utils';
import {
  AddLogicalModelFormData,
  addLogicalModelValidationSchema,
} from './validationSchema';

export type AddLogicalModelDialogProps = {
  defaultValues?: Partial<AddLogicalModelFormData>;
  onCancel?: () => void;
  onSubmit?: (data: AddLogicalModelFormData) => void;
  disabled?: CreateBooleanMap<
    AddLogicalModelFormData & {
      callToAction?: boolean;
    }
  >;
  asDialog?: boolean;
};

type WidgetUIProps = {
  sourceOptions: {
    value: string;
    label: string;
  }[];
  isThereBigQueryOrMssqlSource: boolean;
  modelsAndQueries: {
    queries: NativeQueryWithSource[];
    models: LogicalModelWithSource[];
  };
  source: Source | undefined;
};

// data fetching, bound to UI component
const DataBoundWidgetUI = (props: AddLogicalModelDialogProps) => {
  const {
    Form,
    methods: { watch },
  } = useConsoleForm({
    schema: addLogicalModelValidationSchema,
    options: {
      defaultValues: props.defaultValues,
    },
  });

  const selectedDataSource = watch('dataSourceName');

  const allowedDrivers = useSupportedDriversForNativeQueries();

  const capabilitiesResult = useAllDriverCapabilities({
    select: data => {
      return data
        .filter(
          source =>
            allowedDrivers.includes(source.driver.kind) ||
            supportsSchemaLessTables(source.capabilities)
        )
        .map(({ driver }) => ({
          value: driver.name,
          label: driver.name,
        }));
    },
  });

  const metadataSelector: (
    m: Metadata
  ) => Omit<WidgetUIProps, 'typeOptions' | 'sourceOptions'> = useCallback(
    (m: Metadata) => {
      return {
        isThereBigQueryOrMssqlSource: !!m.metadata.sources.find(
          s => s.kind === 'mssql' || s.kind === 'bigquery'
        ),
        modelsAndQueries: extractModelsAndQueriesFromMetadata(m),
        source: MetadataSelectors.findSource(selectedDataSource)(m),
      };
    },
    [selectedDataSource]
  );

  const metadataResult = useMetadata(metadataSelector);

  if (!metadataResult.isSuccess || !capabilitiesResult.isSuccess)
    return (
      <ReactQueryStatusUI
        status={multipleQueryUtils.status([metadataResult, capabilitiesResult])}
        error={multipleQueryUtils.firstError([
          metadataResult,
          capabilitiesResult,
        ])}
      />
    );

  return (
    <Form
      onSubmit={() => {
        //handled in children:
      }}
    >
      <WidgetUI
        {...props}
        {...metadataResult.data}
        sourceOptions={capabilitiesResult.data}
      />
    </Form>
  );
};

// UI component
const WidgetUI = ({
  modelsAndQueries,
  source,
  sourceOptions,
  isThereBigQueryOrMssqlSource,
  ...props
}: AddLogicalModelDialogProps & WidgetUIProps) => {
  const { handleSubmit } = useFormContext<AddLogicalModelFormData>();

  const isEditMode = !!props.defaultValues?.name;

  const { trackLogicalModel, isLoading: isTracking } = useTrackLogicalModel();

  const logicalModels = modelsAndQueries?.models || [];

  const nameIsLocked = useMemo(() => {
    return (
      isEditMode &&
      findReferencedEntities({
        logicalModelName: props.defaultValues?.name ?? '',
        source,
      }).tables.length > 0
    );
  }, [isEditMode, props.defaultValues?.name, source]);

  const disabledFields = {
    ...props.disabled,
    name: nameIsLocked ?? props.disabled?.name,
  };

  const onSubmit = (data: AddLogicalModelFormData) => {
    let editDetails: Parameters<typeof trackLogicalModel>[0]['editDetails'];

    if (isEditMode) {
      if (!props.defaultValues?.name) {
        throw new Error(
          'Cannot update Logical Model. Unable to find initial name value.'
        );
      }
      editDetails = { originalName: props.defaultValues.name };
    }

    trackLogicalModel({
      data: {
        dataSourceName: data.dataSourceName,
        name: data.name,
        fields: data.fields.map(formFieldToLogicalModelField),
      },
      editDetails,
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: isEditMode
            ? LOGICAL_MODEL_EDIT_SUCCESS
            : LOGICAL_MODEL_CREATE_SUCCESS,
        });
        props.onSubmit?.(data);
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: isEditMode
            ? LOGICAL_MODEL_EDIT_ERROR
            : LOGICAL_MODEL_CREATE_ERROR,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });
  };

  const referencedEntitiesUI = () => {
    const name = props.defaultValues?.name;

    if (!isEditMode || !name) return null;

    const entities = findReferencedEntities({
      source,
      logicalModelName: name,
    });

    return (
      <div className="mb-3">
        <Collapsible
          triggerChildren={
            <div className="font-semibold flex flex-row gap-2">
              <span>Used By </span>
              <Badge color={entities.count > 0 ? 'blue' : 'gray'}>
                {entities.count}
              </Badge>
            </div>
          }
        >
          {!entities.count && (
            <div>
              This Logical Model is not references by any other entities.
            </div>
          )}
          <div className="ml-3">
            <DisplayReferencedLogicalModelEntities entities={entities} />
          </div>
        </Collapsible>
      </div>
    );
  };

  return (
    <>
      <div className="px-md">
        {referencedEntitiesUI()}

        <LogicalModelFormInputs
          sourceOptions={sourceOptions}
          disabled={disabledFields}
          logicalModels={logicalModels}
          nameIsLocked={nameIsLocked}
          isThereBigQueryOrMssqlSource={isThereBigQueryOrMssqlSource}
        />
      </div>
      {props.asDialog ? (
        <Dialog.Footer
          onSubmit={() => handleSubmit(onSubmit)()}
          onClose={props.onCancel}
          isLoading={isTracking}
          callToDeny={'Cancel'}
          callToAction={'Create Logical Model'}
          onSubmitAnalyticsName={'actions-tab-generate-types-submit'}
          onCancelAnalyticsName={'actions-tab-generate-types-cancel'}
          className="sticky w-full bottom-0 left-0"
        />
      ) : (
        <div className="flex justify-end">
          <Button
            disabled={disabledFields.callToAction}
            type="button"
            mode="primary"
            icon={<FaSave />}
            onClick={() => {
              handleSubmit(onSubmit)();
            }}
            isLoading={isTracking}
          >
            {isEditMode ? 'Save' : 'Create'}
          </Button>
        </div>
      )}
    </>
  );
};

// this is what's exported, and it renders the databound UI within a dialog or not
export const LogicalModelWidget = (props: AddLogicalModelDialogProps) => {
  if (props.asDialog) {
    return (
      <Dialog
        size="xl"
        description="Creating a logical model in advance can help generate Native Queries faster"
        title="Add Logical Model"
        hasBackdrop
        onClose={props.onCancel}
      >
        <DataBoundWidgetUI {...props} />
      </Dialog>
    );
  } else {
    return <DataBoundWidgetUI {...props} />;
  }
};
