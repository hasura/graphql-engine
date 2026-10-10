import { useCallback, useMemo } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaSave } from 'react-icons/fa';
import {
  Badge,
  Button,
  Collapsible,
  Dialog,
  useConsoleForm,
  hasuraToast,
  DisplayToastErrorMessage,
  DialogFooter,
  Text,
} from '@hasura/shared/ui';
import { useMetadata } from '@hasura/metadata/api';
import {
  MetadataSelectors,
  LogicalModelWithSource,
  NativeQueryWithSource,
} from '@hasura/metadata/helpers';
import { Metadata, Source } from '@hasura/shared/types';
import { ReactQueryStatusUI } from '../../components';
import { multipleQueryUtils } from '../../components/ReactQueryWrappers/utils';
import { useTrackLogicalModel } from '../../hooks/useTrackLogicalModel';
import { DisplayReferencedLogicalModelEntities } from '../LogicalModel/DisplayLogicalModelReferencedEntities';
import { findReferencedEntities } from '../LogicalModel/utils/findReferencedEntities';
import {
  LOGICAL_MODEL_CREATE_ERROR,
  LOGICAL_MODEL_CREATE_SUCCESS,
  LOGICAL_MODEL_EDIT_ERROR,
  LOGICAL_MODEL_EDIT_SUCCESS,
} from '../constants';
import { LogicalModelFormInputs } from './components/LogicalModelFormInputs';
import { formFieldToLogicalModelField } from './mocks/utils/formFieldToLogicalModelField';
import {
  AddLogicalModelFormData,
  addLogicalModelValidationSchema,
} from './validationSchema';
import { CreateBooleanMap } from '@hasura/shared/types';
import {
  useAllDriverCapabilities,
  supportsSchemaLessTables,
} from '@hasura/metadata/data-source';
import { Flex } from '@radix-ui/themes';

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

  const capabilitiesResult = useAllDriverCapabilities({
    select: (data) => {
      return data
        .filter((source) => supportsSchemaLessTables(source.capabilities))
        .map(({ driver }) => driver);
    },
  });

  const metadataSelector: (
    m: Metadata,
  ) => Omit<WidgetUIProps, 'typeOptions' | 'sourceOptions'> = useCallback(
    (m: Metadata) => {
      return {
        isThereBigQueryOrMssqlSource: !!m.metadata.sources.find(
          (s) => s.kind === 'mssql' || s.kind === 'bigquery',
        ),
        modelsAndQueries:
          MetadataSelectors.extractModelsAndQueriesFromMetadata(m),
        source: MetadataSelectors.findSource(selectedDataSource)(m),
      };
    },
    [selectedDataSource],
  );

  const metadataResult = useMetadata();

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

  const metadataProps = metadataSelector(metadataResult.data);
  const sourceOptions = metadataResult.data.metadata.sources
    .filter((source) => capabilitiesResult.data.includes(source.kind))
    .map((source) => ({
      value: source.name,
      label: source.name,
    }));

  return (
    <Form
      onSubmit={() => {
        //handled in children:
      }}
    >
      <WidgetUI {...props} {...metadataProps} sourceOptions={sourceOptions} />
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

  const { trackLogicalModel, isPending: isTracking } = useTrackLogicalModel();

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
          'Cannot update Logical Model. Unable to find initial name value.',
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
      onError: (err) => {
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
            <Flex direction="row" gap="2">
              <Text weight="bold">Used By </Text>
              <Badge color={entities.count > 0 ? 'blue' : 'gray'}>
                {entities.count}
              </Badge>
            </Flex>
          }
        >
          {!entities.count && (
            <Text as="div">
              This Logical Model is not references by any other entities.
            </Text>
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
      <div>
        {referencedEntitiesUI()}

        <LogicalModelFormInputs
          source={source}
          sourceOptions={sourceOptions}
          disabled={disabledFields}
          logicalModels={logicalModels}
          nameIsLocked={nameIsLocked}
          isThereBigQueryOrMssqlSource={isThereBigQueryOrMssqlSource}
        />
      </div>
      {props.asDialog ? (
        <DialogFooter
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
        <Flex justify="end" className="mt-4">
          <Button
            disabled={disabledFields.callToAction}
            type="button"
            mode="primary"
            leftIcon={FaSave}
            onClick={() => {
              handleSubmit(onSubmit)();
            }}
            loading={isTracking}
          >
            {isEditMode ? 'Save' : 'Create'}
          </Button>
        </Flex>
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
        onClose={props.onCancel}
      >
        <DataBoundWidgetUI {...props} />
      </Dialog>
    );
  } else {
    return <DataBoundWidgetUI {...props} />;
  }
};
