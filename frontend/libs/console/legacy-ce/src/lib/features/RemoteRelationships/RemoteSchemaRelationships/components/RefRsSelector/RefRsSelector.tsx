import React, { useEffect } from 'react';
import { FaKey, FaPlug } from 'react-icons/fa';
import {
  Card,
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
} from '@hasura/shared/ui';
import { useFormContext } from 'react-hook-form';
import { useIntrospectRemoteSchema } from '@hasura/metadata/api';

export interface RefRsSelectorProps {
  allRemoteSchemas: string[];
}

export const refRemoteSchemaSelectorKey = 'referenceRemoteSchema';
export const refRemoteOperationSelectorKey = 'selectedOperation';

export const RefRsSelector = ({ allRemoteSchemas }: RefRsSelectorProps) => {
  const { setValue, watch, setError, clearErrors } = useFormContext();

  const selectedOperation = watch(refRemoteOperationSelectorKey);
  const referenceRemoteSchema = watch(refRemoteSchemaSelectorKey);
  const { data, isLoading, isError } = useIntrospectRemoteSchema(
    referenceRemoteSchema,
  );

  // The available operations are fully derived from the introspected schema.
  // While a new reference schema is loading, `data` is undefined (the query key
  // changes), so this naturally resets to `[]` — matching the previous explicit
  // reset-on-schema-change behavior without mirroring into state via effects.
  const operations = React.useMemo(
    () => Object.keys(data?.getQueryType()?.getFields() || {}),
    [data],
  );

  const rsOptions = React.useMemo(
    () => allRemoteSchemas.map((t) => ({ value: t, label: t })),
    [allRemoteSchemas],
  );

  // When the loaded schema no longer offers the currently selected operation,
  // clear it. Guarded by `selectedOperation` so re-running after the reset (now
  // that `selectedOperation` is a dependency) does not write '' again.
  useEffect(() => {
    if (
      selectedOperation &&
      operations.length > 0 &&
      !operations.includes(selectedOperation)
    ) {
      setValue(refRemoteOperationSelectorKey, '');
    }
  }, [operations, selectedOperation, setValue]);

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
  }, [isError, clearErrors, setError, setValue]);

  useEffect(() => {
    setValue('resultSet', { [selectedOperation]: { arguments: {} } });
  }, [setValue, selectedOperation]);

  return (
    <Card className="border-l-4 border-l-indigo-900 h-full">
      <div className="mb-2 w-full">
        <ReactSelectField
          label="Target Remote Schema"
          name={refRemoteSchemaSelectorKey}
          placeholder="Select a remote schema"
          options={rsOptions}
          labelIcon={FaPlug}
          dataTest="select-ref-rs"
          selectProps={REACT_SELECT_FILTER_PROPS}
        />
      </div>
      <div className="mb-2 w-full">
        <ReactSelectField
          disabled={!referenceRemoteSchema || isLoading || isError}
          placeholder={isLoading ? 'Loading...' : 'Select a reference field'}
          label="Target Remote Schema Field"
          name={refRemoteOperationSelectorKey}
          options={operations.map((t) => ({ value: t, label: t }))}
          labelIcon={FaKey}
          selectProps={REACT_SELECT_FILTER_PROPS}
        />
      </div>
    </Card>
  );
};
