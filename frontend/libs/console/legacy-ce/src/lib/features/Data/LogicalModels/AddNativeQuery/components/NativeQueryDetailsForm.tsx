import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';
import {
  GraphQLSanitizedInputField,
  InputField,
  Select,
} from '../../../../../new-components/Form';
import { LimitedFeatureWrapper } from '../../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import { useMetadata } from '../../../../hasura-metadata-api';
import { ReactQueryStatusUI } from '../../../components/ReactQueryWrappers/ReactQueryStatusUI';
import { Source } from '../../../../hasura-metadata-types';
import { useSupportedDataTypes } from '../../../hooks/useSupportedDataTypes';
import { LogicalModelWidget } from '../../LogicalModelWidget/LogicalModelWidget';
import { ArgumentsField } from '../components/ArgumentsField';
import { SqlEditorField } from '../components/SqlEditorField';
import { NativeQueryForm } from '../types';
import { ReactQueryUIWrapper } from '../../../components';

export const NativeQueryFormFields = ({ sources }: { sources?: Source[] }) => {
  const { watch, setValue } = useFormContext<NativeQueryForm>();
  const selectedSource = watch('source');

  const logicalModels = sources?.find(
    s => s.name === selectedSource
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

  const { data: isThereBigQueryOrMssqlSource } = useMetadata(
    m =>
      !!m.metadata.sources.find(
        s => s.kind === 'mssql' || s.kind === 'bigquery'
      )
  );

  const [isLogicalModelsDialogOpen, setIsLogicalModelsDialogOpen] =
    React.useState(false);

  /**
   * Options for the data source types
   */
  const supportedDataTypesResult = useSupportedDataTypes({
    dataSourceName: selectedSource,
    options: {
      enabled: !!selectedSource,
    },
  });

  return (
    <>
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
          options={(sources ?? []).map(m => ({
            label: m.name,
            value: m.name,
          }))}
          placeholder="Select a database..."
        />
      </div>
      <div className="max-w-4xl">
        {isThereBigQueryOrMssqlSource && (
          <LimitedFeatureWrapper
            title="Looking to add Native Queries for SQL Server/Big Query databases?"
            id="native-queries"
            description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
          />
        )}
      </div>
      <ReactQueryUIWrapper
        loadingStyle="overlay"
        loader="spinner"
        fallbackData={[]}
        miniSpinnerBackdrop
        useQueryResult={supportedDataTypesResult}
        render={({ data: typeOptions }) => (
          <ArgumentsField
            noSourceSelected={!selectedSource}
            types={typeOptions}
          />
        )}
      />

      <SqlEditorField />
      <div className="flex w-full">
        {/* Logical Model Dropdown */}
        <Select
          name="returns"
          selectClassName="max-w-xl"
          label="Query Return Type"
          placeholder={logicalModelSelectPlaceholder()}
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
          defaultValues={{ dataSourceName: selectedSource }}
          disabled={{ dataSourceName: !!selectedSource }}
          onCancel={() => {
            setIsLogicalModelsDialogOpen(false);
          }}
          onSubmit={data => {
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
