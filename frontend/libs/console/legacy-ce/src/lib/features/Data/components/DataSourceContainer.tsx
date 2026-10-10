import { Outlet, useParams } from 'react-router';
import { useMetadata } from '@hasura/metadata/api';
import { DataSourceContext } from '../context/DataSourceContext';
import { SkeletonList, Text } from '@hasura/shared/ui';
import { Em } from '@radix-ui/themes';

type Params = {
  source?: string;
  schema?: string;
};

const DataSourceContainer = () => {
  const { data: meta, isLoading: metadataLoading } = useMetadata();
  const { source, schema } = useParams<Params>();
  const currentSource =
    source && meta
      ? meta.metadata?.sources?.find((s) => s.name === source)
      : undefined;

  if (!meta) {
    if (metadataLoading) {
      return <SkeletonList count={5} />;
    }

    return (
      <Text>
        <Em>Failed to load metadata. Reload to retry again</Em>
      </Text>
    );
  }

  if (!currentSource || !schema) {
    return (
      <Text>
        <Em>Data source {source} or schema doesn&apos;t exist</Em>
      </Text>
    );
  }

  return (
    <DataSourceContext.Provider
      value={{
        ...meta,
        currentSource,
      }}
    >
      <Outlet />
    </DataSourceContext.Provider>
  );
};

export default DataSourceContainer;
