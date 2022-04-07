import React from 'react';
import { FaPlug } from 'react-icons/fa';
import { Select } from '@/new-components/Form';

export interface RefRsSelectorProps {
  allRemoteSchemas: string[];
}

export const refRemoteSchemaSelectorKey = 'referenceRemoteSchema';

export const RefRsSelector = ({ allRemoteSchemas }: RefRsSelectorProps) => {
  const rsOptions = React.useMemo(
    () => allRemoteSchemas.map(t => ({ value: t, label: t })),
    [allRemoteSchemas]
  );

  return (
    <div className="bg-gray-50 rounded p-md border border-gray-300 border-l-4 border-l-indigo-900 h-full">
      <div className="mb-sm w-full">
        <Select
          label="Reference Remote Schema"
          name={refRemoteSchemaSelectorKey}
          placeholder="Select a remote schema"
          options={rsOptions}
          labelIcon={<FaPlug />}
          dataTest="select-ref-rs"
        />
      </div>
    </div>
  );
};
