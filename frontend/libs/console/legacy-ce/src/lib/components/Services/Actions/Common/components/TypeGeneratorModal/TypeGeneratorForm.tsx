import React, { useEffect } from 'react';
import z from 'zod';
import { CodeEditorField } from '../../../../../../new-components/Form';
import { formatSdl } from 'format-graphql';
import { jsonToSchema } from '@walmartlabs/json-to-simple-graphql-schema/lib';
import { useFormContext } from 'react-hook-form';
import { useDebouncedEffect } from '../../../../../../hooks/useDebounceEffect';
import { SchemaType } from './types';

export const schema = z.object({
  jsonInput: z.string(),
  graphqlInput: z.string(),
  jsonOutput: z.string(),
  graphqlOutput: z.string(),
});

const editorOptions = {
  minLines: 10,
  maxLines: 10,
  showLineNumbers: true,
  useSoftTabs: true,
};

export const TypeGeneratorForm = (props: {
  setValues: (values: SchemaType) => void;
}) => {
  const { setValues } = props;
  const { watch, setValue, setError, clearErrors } = useFormContext();
  const jsonInput = watch('jsonInput');
  const jsonOutput = watch('jsonOutput');
  const graphqlInput = watch('graphqlInput');
  const graphqlOutput = watch('graphqlOutput');

  useEffect(() => {
    setValues({
      jsonInput,
      jsonOutput,
      graphqlInput,
      graphqlOutput,
    });
  }, [jsonInput, jsonOutput, graphqlInput, graphqlOutput]);

  useDebouncedEffect(
    () => {
      clearErrors();
      try {
        if (jsonInput) {
          const schema = jsonToSchema({
            jsonInput: jsonInput,
            baseType: 'SampleInput',
          });
          setValue(
            'graphqlInput',
            formatSdl(schema.value.replace(/type\s(.*)\s\{/g, 'input $1 {'))
          );
        }
      } catch (e) {
        setError('jsonInput', {
          message: 'Invalid JSON',
        });
      }

      try {
        if (jsonOutput) {
          const schema = jsonToSchema({
            jsonInput: jsonOutput,
            baseType: 'SampleOutput',
          });
          setValue('graphqlOutput', formatSdl(schema.value));
        }
      } catch (e) {
        setError('jsonOutput', {
          message: 'Invalid JSON',
        });
      }
    },
    400,
    [jsonInput, jsonOutput, clearErrors, setError, setValue]
  );

  return (
    <div>
      <div className="flex">
        <CodeEditorField
          noErrorPlaceholder
          name="jsonInput"
          label="Sample Request (JSON)"
          tooltip="Enter a sample request in JSON format to generate the input type"
          editorOptions={editorOptions}
          editorProps={{
            className: 'rounded-r-none',
          }}
        />
        <CodeEditorField
          name="graphqlInput"
          label="&nbsp;"
          tooltip=""
          editorOptions={editorOptions}
          editorProps={{
            className: 'rounded-l-none bg-slate-100',
          }}
        />
      </div>
      <div className="flex">
        <CodeEditorField
          name="jsonOutput"
          label="Sample Response (JSON)"
          tooltip="Enter a sample response in JSON format to generate the output type"
          editorOptions={editorOptions}
          editorProps={{
            className: 'rounded-r-none',
          }}
        />
        <CodeEditorField
          name="graphqlOutput"
          label="&nbsp;"
          tooltip=""
          editorOptions={editorOptions}
          editorProps={{
            className: 'rounded-l-none bg-slate-100',
          }}
        />
      </div>
    </div>
  );
};
