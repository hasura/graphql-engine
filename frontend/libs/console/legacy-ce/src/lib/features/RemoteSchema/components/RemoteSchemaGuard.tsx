import { Outlet, useParams } from 'react-router';
import { useMetadata } from '@hasura/metadata/api';
import { CurrentRemoteSchemaContext } from '../context';

const RemoteSchemaGuard = () => {
  const params = useParams();
  const { data: meta, isFetching, error } = useMetadata();

  if (isFetching && !meta) {
    return <div>Loading...</div>;
  }

  if (!meta || error) {
    return (
      <div>Error happened when fetching metadata. Reload to retry again</div>
    );
  }

  const currentRemoteSchema = params.remoteSchemaName
    ? meta.metadata.remote_schemas?.find(
        (rm) => rm.name === params.remoteSchemaName,
      )
    : undefined;

  if (!currentRemoteSchema) {
    return <div>Remote schema not found</div>;
  }

  return (
    <CurrentRemoteSchemaContext.Provider
      value={{
        ...meta,
        currentRemoteSchema,
      }}
    >
      <Outlet />
    </CurrentRemoteSchemaContext.Provider>
  );
};

export default RemoteSchemaGuard;
