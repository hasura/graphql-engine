import { FaDatabase } from 'react-icons/fa';
import { Card, IndicatorCard, ReactSelectField } from '@hasura/shared/ui';
import { useSourceOptions } from '../RemoteSchemaToDB/hooks';
import { Metadata } from '@hasura/shared/types';

export const RemoteDatabaseWidget = ({ meta }: { meta: Metadata }) => {
  const {
    data: sourceOptions,
    isError: sourcesError,
    isFetching,
  } = useSourceOptions(meta);

  if (sourcesError) {
    return (
      <IndicatorCard status="negative">Error loading database</IndicatorCard>
    );
  }

  return (
    <Card className="col-span-5 border-l-4 border-l-indigo-600 w-full">
      <ReactSelectField
        label="Target"
        name="target"
        options={sourceOptions ?? []}
        dataTest="select-ref-db"
        labelIcon={FaDatabase}
        noErrorPlaceholder
        loading={isFetching}
        selectProps={{
          filterOption: (option, filterValue) => {
            return (
              !filterValue ||
              option.label.toLowerCase().includes(filterValue.toLowerCase())
            );
          },
        }}
      />
    </Card>
  );
};
