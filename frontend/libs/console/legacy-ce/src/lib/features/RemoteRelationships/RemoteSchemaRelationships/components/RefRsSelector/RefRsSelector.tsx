import React, { useEffect } from 'react';
import { FaKey, FaPlug } from 'react-icons/fa';
import { SelectField } from '../../../../../new-components/Form';
import { useFormContext } from 'react-hook-form';
import { useRemoteSchema } from '../../../../MetadataAPI';

export interface RefRsSelectorProps {
  allRemoteSchemas: string[];
}

export const refRemoteSchemaSelectorKey = 'referenceRemoteSchema';
export const refRemoteOperationSelectorKey = 'selectedOperation';

export const RefRsSelector = ({ allRemoteSchemas }: RefRsSelectorProps) => {
  const { fetchSchema, data, isLoading, isError } = useRemoteSchema();

  const [operations, setOperations] = React.useState<string[]>([]);

  const rsOptions = React.useMemo(
    () => allRemoteSchemas.map(t => ({ value: t, label: t })),
    [allRemoteSchemas]
  );

  const { setValue, watch, setError, clearErrors } = useFormContext();

  const selectedOperation = watch(refRemoteOperationSelectorKey);
  const referenceRemoteSchema = watch(refRemoteSchemaSelectorKey);

  useEffect(() => {
    if (referenceRemoteSchema) {
      fetchSchema(referenceRemoteSchema);
      setOperations([]);
    }
  }, [referenceRemoteSchema]);

  useEffect(() => {
    const operations = Object.keys(data?.getQueryType()?.getFields() || {});
    setOperations(operations);
    if (operations.length > 0 && !operations.includes(selectedOperation)) {
      setValue(refRemoteOperationSelectorKey, '');
    }
  }, [data]);

  useEffect(() => {
    if (isError) {
      setValue('resultSet', null);
      setError(refRemoteOperationSelectorKey, {
        type: 'manual',
        message: 'Error fetching remote schema',
      });
    } else {
      clearErrors(refRemoteOperationSelectorKey);
    }
  }, [isError]);

  useEffect(() => {
    setValue('resultSet', { [selectedOperation]: { arguments: {} } });
  }, [setValue, selectedOperation]);

  return (
    <div className="bg-gray-50 rounded p-md border border-gray-300 border-l-4 border-l-indigo-900 h-full">
      <div className="mb-sm w-full">
        <SelectField
          label="Target Remote Schema"
          name={refRemoteSchemaSelectorKey}
          placeholder="Select a remote schema"
          options={rsOptions}
          labelIcon={<FaPlug />}
          dataTest="select-ref-rs"
        />
      </div>
      <div className="mb-sm w-full">
        <SelectField
          disabled={!referenceRemoteSchema || isLoading || isError}
          placeholder={isLoading ? 'Loading...' : 'Select a reference field'}
          label="Target Remote Schema Field"
          name={refRemoteOperationSelectorKey}
          options={operations.map(t => ({ value: t, label: t }))}
          labelIcon={<FaKey style={{ strokeWidth: 4.5 }} />}
        />
      </div>
    </div>
  );
};
