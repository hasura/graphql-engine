import React from 'react';
import { FaPlusCircle, FaSave } from 'react-icons/fa';
import { Button } from '../../../../new-components/Button';
import {
  GraphQLSanitizedInputField,
  InputField,
  Select,
  useConsoleForm,
} from '../../../../new-components/Form';
// import { FormDebugWindow } from '../../../../new-components/Form/dev-components/FormDebugWindow';
import Skeleton from 'react-loading-skeleton';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { hasuraToast } from '../../../../new-components/Toasts';
import {
  useEnvironmentState,
  usePushRoute,
} from '../../../ConnectDBRedesign/hooks';
import { Feature, nativeDrivers } from '../../../DataSource';
import { useMetadata } from '../../../hasura-metadata-api';
import { useSupportedDataTypes } from '../../hooks/useSupportedDataTypes';
import { useTrackNativeQuery } from '../../hooks/useTrackNativeQuery';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';
import { ArgumentsField } from './components/ArgumentsField';
import { SqlEditorField } from './components/SqlEditorField';
import { schema } from './schema';
import { NativeQueryForm } from './types';
import { transformFormOutputToMetadata } from './utils';

type AddNativeQueryProps = {
  defaultFormValues?: Partial<NativeQueryForm>;
  mode?: 'create' | 'update';
};

export const AddNativeQuery = ({
  defaultFormValues,
  mode = 'create',
}: AddNativeQueryProps) => {
  const {
    Form,
    methods: { watch, setValue },
  } = useConsoleForm({
    schema,
    options: { defaultValues: defaultFormValues },
  });

  const push = usePushRoute();

  const { consoleType } = useEnvironmentState();
  const allowedDrivers = consoleType === 'oss' ? ['postgres'] : nativeDrivers;

  const {
    data: sources,
    isLoading: isSourcesLoading,
    error: sourcesError,
  } = useMetadata(s => {
    return s.metadata.sources.filter(s => allowedDrivers.includes(s.kind));
  });

  const selectedSource = watch('source');

  React.useEffect(() => {
    const subscription = watch((value, { name, type }) => {
      if (name === 'source' && type === 'change') {
        // onChange fired for the source field
        // reset the "returns" value if the source changes
        setValue('returns', '');
      }
    });
    return () => subscription.unsubscribe();
  }, [watch]);

  const logicalModels = sources?.find(
    s => s.name === selectedSource
  )?.logical_models;

  const { trackNativeQuery, isLoading: isSaving } = useTrackNativeQuery();

  const [isLogicalModelsDialogOpen, setIsLogicalModelsDialogOpen] =
    React.useState(false);

  const handleFormSubmit = (values: NativeQueryForm) => {
    const metadataNativeQuery = transformFormOutputToMetadata(values);

    trackNativeQuery({
      data: { ...metadataNativeQuery, source: values.source },
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          message: `Successfully tracked native query as: ${values.root_field_name}`,
          title: 'Track Native Query',
          toastOptions: { duration: 3000 },
        });
        // Go to list
        push('/data/native-queries');
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          message: err.message,
          title: 'Track Native Query',
          //toastOptions: { duration: 2000 },
        });
      },
    });
  };

  const logicalModelSelectPlaceholder = () => {
    if (!selectedSource) {
      return 'Select a database first...';
    } else if (!!selectedSource && (logicalModels ?? []).length === 0) {
      return `No logical models found for ${selectedSource}.`;
    } else {
      return `Select a logical model...`;
    }
  };

  /**
   * Options for the data source types
   */
  const {
    data: typeOptions = [],
    error: typeOptionError,
    isLoading: isIntrospectionLoading,
  } = useSupportedDataTypes({
    dataSourceName: selectedSource,
    select: values => {
      if (values === Feature.NotImplemented) return [];
      return Object.values(values).flat();
    },
    options: {
      enabled: !!selectedSource,
    },
  });

  if (sourcesError || typeOptionError)
    return (
      <IndicatorCard status="negative" headline="Internal Error">
        <div>{sourcesError}</div>
        <div> {typeOptionError?.message}</div>
      </IndicatorCard>
    );

  return (
    <div>
      {mode === 'update' && (
        <IndicatorCard status="info">
          The current release does not support editing Native Queries. This
          feature will be available in a future release. You can still edit
          directly by modifying the Metadata.
        </IndicatorCard>
      )}
      <Form onSubmit={handleFormSubmit}>
        <fieldset disabled={mode === 'update'}>
          {/* <FormDebugWindow /> */}
          <div className="max-w-xl flex flex-col">
            <GraphQLSanitizedInputField
              name="root_field_name"
              label="Native Query Name"
              placeholder="Name that exposes this model in GraphQL API"
              hideTips
            />
            <InputField
              name="comment"
              label="Comment"
              placeholder="A description of this logical model"
            />
            <Select
              name="source"
              label="Database"
              // saving prop for future update
              //noOptionsMessage="No databases found."
              loading={isSourcesLoading}
              options={(sources ?? []).map(m => ({
                label: m.name,
                value: m.name,
              }))}
              placeholder="Select a database..."
            />
          </div>
          {isIntrospectionLoading ? (
            <div>
              <Skeleton />
              <Skeleton />
            </div>
          ) : (
            <ArgumentsField types={typeOptions} />
          )}
          <SqlEditorField />
          <div className="flex w-full">
            {/* Logical Model Dropdown */}
            <Select
              name="returns"
              selectClassName="max-w-xl"
              // saving prop for future update
              // noOptionsMessage={
              //   !selectedSource ? 'Select a database first.' : 'No models found.'
              // }
              // force component re-init on source change
              //key={selectedSource}
              label="Query Return Type"
              placeholder={logicalModelSelectPlaceholder()}
              loading={isSourcesLoading}
              options={(logicalModels ?? []).map(m => ({
                label: m.name,
                value: m.name,
              }))}
            />
            <Button
              icon={<FaPlusCircle />}
              onClick={e => {
                setIsLogicalModelsDialogOpen(true);
              }}
            >
              Add Logical Model
            </Button>
          </div>
          {isLogicalModelsDialogOpen ? (
            <LogicalModelWidget
              onCancel={() => {
                setIsLogicalModelsDialogOpen(false);
              }}
              onSubmit={() => {
                setIsLogicalModelsDialogOpen(false);
              }}
              asDialog
            />
          ) : null}
          <div className="flex flex-row justify-end gap-2">
            {/*
              Validate Button will remain hidden until we have more information about how to handle standalone validation
              Slack thread: https://hasurahq.slack.com/archives/C04LV93JNSH/p1682965503376129
          */}
            {/* <Button icon={<FaPlay />}>Validate</Button> */}
            <Button
              type="submit"
              icon={<FaSave />}
              mode="primary"
              isLoading={isSaving}
            >
              Save
            </Button>
          </div>
        </fieldset>
      </Form>
    </div>
  );
};
