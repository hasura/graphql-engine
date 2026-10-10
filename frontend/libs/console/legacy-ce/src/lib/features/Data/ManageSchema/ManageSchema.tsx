import { useGetTableSchemas } from '@hasura/metadata/data-source';
import { Source } from '@hasura/shared/types';
import { IndicatorCard, SkeletonList } from '@hasura/shared/ui';
import React from 'react';
import ManageSchemaUI from './ManageSchemaUI';

const ManageSchema: React.FC<{
  source: Source;
}> = ({ source }) => {
  const {
    error,
    data: schemaList,
    isFetching,
    refetch,
  } = useGetTableSchemas({
    source,
  });

  if (isFetching) {
    return <SkeletonList count={5} />;
  }

  if (error || !schemaList) {
    return (
      <IndicatorCard status="negative" showIcon>
        Failed to fetch database schemas tables. Please try again later.
      </IndicatorCard>
    );
  }

  return (
    <ManageSchemaUI source={source} schemaList={schemaList} refetch={refetch} />
  );
};

export default ManageSchema;
