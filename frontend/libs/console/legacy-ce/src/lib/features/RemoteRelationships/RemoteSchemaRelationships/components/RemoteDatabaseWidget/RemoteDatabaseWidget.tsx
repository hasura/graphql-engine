import React from 'react';
import { FaDatabase } from 'react-icons/fa';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { SourceSelect } from './SourceSelect';
import { useSourceOptions } from '../RemoteSchemaToDB/hooks';

export const RemoteDatabaseWidget = () => {
  const { data: sourceOptions, isError: sourcesError } = useSourceOptions();

  if (sourcesError) {
    return (
      <IndicatorCard status="negative">Error loading database</IndicatorCard>
    );
  }

  return (
    <div className="bg-gray-50 col-span-5 rounded p-md border border-gray-300 border-l-4 border-l-indigo-600">
      <div className="mb-sm">
        <SourceSelect
          label="Target"
          name="target"
          options={sourceOptions ?? []}
          dataTest="select-ref-db"
          labelIcon={<FaDatabase />}
        />
      </div>
    </div>
  );
};
