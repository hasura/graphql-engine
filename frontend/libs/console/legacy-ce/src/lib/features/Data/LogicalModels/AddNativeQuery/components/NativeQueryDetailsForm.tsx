import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import {
  Button,
  CodeEditorField,
  GraphQLSanitizedInputField,
  InputField,
  SelectField,
  Text,
} from '@hasura/shared/ui';
import { LimitedFeatureWrapper } from '../../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import { useMetadata } from '@hasura/metadata/api';
import { Source } from '@hasura/shared/types';
import { LogicalModelWidget } from '../../LogicalModelWidget/LogicalModelWidget';
import { ArgumentsField } from '../components/ArgumentsField';
import { NativeQueryForm } from '../types';
import { useSupportedScalars } from '@hasura/metadata/data-source';
import { Flex } from '@radix-ui/themes';

export const NativeQueryFormFields = ({ sources }: { sources?: Source[] }) => {
  const { watch, setValue } = useFormContext<NativeQueryForm>();
  const selectedSource = watch('source');

  const logicalModels = sources?.find(
    (s) => s.name === selectedSource,
  )?.logical_models;

  const logicalModelSelectPlaceholder = () => {
    if (!selectedSource) {
      return 'Select a database first...';
    }

    if (logicalModels?.length === 0) {
      return `No logical models found for ${selectedSource}.`;
    }

    return 'Select a logical model...';
  };

  const { data: meta } = useMetadata();

  const [isLogicalModelsDialogOpen, setIsLogicalModelsDialogOpen] =
    React.useState(false);

  const isThereBigQueryOrMssqlSource =
    meta?.metadata?.sources?.some(
      (s) => s.kind === 'mssql' || s.kind === 'bigquery',
    ) ?? false;

  const source = meta?.metadata?.sources.find((s) => s.name === selectedSource);
  /**
   * Options for the data source types
   */
  const { data: supportedDataTypesResult } = useSupportedScalars(source?.kind);

  return (
    <>
      <Flex direction="column" className="max-w-xl" gap="4">
        <GraphQLSanitizedInputField
          name="root_field_name"
          label="Native Query Name"
          hideTips
          fieldProps={{
            placeholder: 'Name that exposes this model in GraphQL API',
          }}
        />
        <InputField
          name="comment"
          label="Comment"
          fieldProps={{ placeholder: 'A description of this logical model' }}
        />
        <SelectField
          name="source"
          label="Database"
          // saving prop for future update
          //noOptionsMessage="No databases found."
          options={(sources ?? []).map((m) => ({
            label: m.name,
            value: m.name,
          }))}
          placeholder="Select a database..."
        />
      </Flex>
      <div className="max-w-4xl">
        {isThereBigQueryOrMssqlSource && (
          <LimitedFeatureWrapper
            title="Looking to add Native Queries for SQL Server/Big Query databases?"
            id="native-queries"
            description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
          />
        )}
      </div>
      <ArgumentsField
        noSourceSelected={!selectedSource}
        types={supportedDataTypesResult ?? []}
      />
      <CodeEditorField
        name="code"
        label="Native Query Statement"
        editorProps={{
          mode: 'sql',
        }}
      />
      <Flex className="w-full">
        {/* Logical Model Dropdown */}
        <SelectField
          name="returns"
          label={
            <>
              <Text weight="medium">Query Return Type</Text>
              <Button
                mode="default"
                size="1"
                leftIcon={FaPlusCircle}
                onClick={() => {
                  setIsLogicalModelsDialogOpen(true);
                }}
              >
                Add Logical Model
              </Button>
            </>
          }
          placeholder={logicalModelSelectPlaceholder()}
          options={(logicalModels ?? []).map((m) => ({
            label: m.name,
            value: m.name,
          }))}
          fieldProps={{
            trigger: {
              className: 'max-w-xl',
            },
          }}
        />
      </Flex>
      {isLogicalModelsDialogOpen ? (
        <LogicalModelWidget
          defaultValues={{ dataSourceName: selectedSource }}
          disabled={{ dataSourceName: !!selectedSource }}
          onCancel={() => {
            setIsLogicalModelsDialogOpen(false);
          }}
          onSubmit={(data) => {
            if (data.dataSourceName !== selectedSource) {
              setValue('source', data.dataSourceName);
            }
            setIsLogicalModelsDialogOpen(false);
          }}
          asDialog
        />
      ) : null}
    </>
  );
};
