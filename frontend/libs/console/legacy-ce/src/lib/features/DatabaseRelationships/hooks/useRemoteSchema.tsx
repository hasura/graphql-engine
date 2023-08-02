import { SchemaResponse } from '../../MetadataAPI';
import { buildClientSchema } from 'graphql';
import { useQuery } from 'react-query';
import { RemoteSchema } from '../../hasura-metadata-types';
import { runMetadataQuery } from '../../DataSource';
import { useHttpClient } from '../../Network';

export const useRemoteSchemaIntrospection = ({
  remoteSchemaName,
  enabled = true,
}: {
  remoteSchemaName: RemoteSchema['name'];
  enabled?: boolean;
}) => {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: ['introspection', remoteSchemaName],
    queryFn: async () => {
      const body = {
        type: 'introspect_remote_schema',
        args: {
          name: remoteSchemaName,
        },
      };
      const result = await runMetadataQuery<SchemaResponse>({
        httpClient,
        body,
      });
      const schema = buildClientSchema(result.data);
      return schema;
    },
    enabled,
  });
};
