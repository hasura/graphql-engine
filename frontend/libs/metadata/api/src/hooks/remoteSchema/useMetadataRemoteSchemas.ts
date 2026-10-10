import { useMetadata } from '../metadata/useMetadata';

export const useListRemoteSchemas = () => {
  return useMetadata(
    (m) => m.metadata.remote_schemas?.map((rs) => rs.name) ?? [],
  );
};
