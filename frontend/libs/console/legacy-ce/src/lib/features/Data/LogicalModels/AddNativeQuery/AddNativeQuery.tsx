import React from 'react';
import { FaSave } from 'react-icons/fa';
import { Button } from '../../../../new-components/Button';
import {
  GraphQLSanitizedInputField,
  InputField,
  Select,
  useConsoleForm,
} from '../../../../new-components/Form';
import { FormDebugWindow } from '../../../../new-components/Form/dev-components/FormDebugWindow';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useMetadata } from '../../../hasura-metadata-api';
import { useTrackNativeQuery } from '../../hooks/useTrackNativeQuery';

import { ArgumentsField } from './components/ArgumentsField';
import { PageWrapper } from './components/PageWrapper';
import { SqlEditorField } from './components/SqlEditorField';
import { schema } from './schema';
import { NativeQueryForm } from './types';
import { transformFormOutputToMetadata } from './utils';

type AddNativeQueryProps = {
  defaultFormValues?: Partial<NativeQueryForm>;
};
export const AddNativeQuery = ({ defaultFormValues }: AddNativeQueryProps) => {
  const {
    Form,
    methods: { watch, setValue },
  } = useConsoleForm({
    schema,
    options: { defaultValues: defaultFormValues },
  });

  const { data: sources, isLoading: isSourcesLoading } = useMetadata(s =>
    s.metadata.sources.filter(s => ['postgres', 'mssql'].includes(s.name))
  );

  const selectedSource = watch('source');

  React.useEffect(() => {
    // if source changes, reset value for logical model since any previously selected value would be invalid because it's not a diff database
    setValue('returns', '');
  }, [selectedSource]);

  const logicalModels = sources?.find(
    s => s.name === selectedSource
  )?.logical_models;

  const { trackNativeQuery } = useTrackNativeQuery();

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

  return (
    <PageWrapper>
      <Form onSubmit={handleFormSubmit}>
        <FormDebugWindow />
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
        <ArgumentsField />
        <SqlEditorField />
        <div className="max-w-xl flex flex-col">
          {/* Logical Model Dropdown */}
          <Select
            name="returns"
            // saving prop for future update
            // noOptionsMessage={
            //   !selectedSource ? 'Select a database first.' : 'No models found.'
            // }
            // force component re-init on source change
            //key={selectedSource}
            label="Query Return Type"
            placeholder={
              !!selectedSource && (logicalModels ?? []).length === 0
                ? 'No models found.'
                : 'Select a logical model...'
            }
            loading={isSourcesLoading}
            options={(logicalModels ?? []).map(m => ({
              label: m.name,
              value: m.name,
            }))}
          />
        </div>
        <div className="flex flex-row justify-end gap-2">
          {/* 
              Validate Button will remain hidden until we have more information about how to handle standalone validation
              Slack thread: https://hasurahq.slack.com/archives/C04LV93JNSH/p1682965503376129
          */}
          {/* <Button icon={<FaPlay />}>Validate</Button> */}
          <Button type="submit" icon={<FaSave />} mode="primary">
            Save
          </Button>
        </div>
      </Form>
    </PageWrapper>
  );
};
