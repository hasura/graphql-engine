import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaPlug } from 'react-icons/fa';
import { FiType } from 'react-icons/fi';
import { Select } from '@/new-components/Form';
import { RsToRsSchema } from '../../types';

export interface RsSourceTypeSelectorProps {
  types: string[];
  sourceTypeKey: string;
}

export const remoteSchemaSelectorKey = 'sourceRemoteSchema';

export const RsSourceTypeSelector = ({
  types,
  sourceTypeKey,
}: RsSourceTypeSelectorProps) => {
  const { watch } = useFormContext<RsToRsSchema>();
  const remoteSchemaName = watch(remoteSchemaSelectorKey);
  const typeOptions = React.useMemo(
    () => types.map(t => ({ value: t, label: t })),
    [types]
  );
  const schemaOptions = [{ value: remoteSchemaName, label: remoteSchemaName }];

  return (
    <div className="bg-gray-50 rounded p-md border border-gray-300 border-l-4 border-l-green-600 w-full h-full">
      <div className="mb-sm w-full">
        <Select
          label="Source Remote Schema"
          name={remoteSchemaSelectorKey}
          placeholder="Select a remote schema"
          options={schemaOptions}
          labelIcon={<FaPlug />}
          disabled
        />
      </div>
      <div className="mb-sm w-full">
        <Select
          label="Source Type"
          name={sourceTypeKey}
          placeholder="Select a type"
          options={typeOptions}
          labelIcon={<FiType style={{ strokeWidth: 4.5 }} />}
          dataTest="select-source-type"
        />
      </div>
    </div>
  );
};
