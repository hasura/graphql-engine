import { useMetadataSource } from '../MetadataAPI';
import { DataTarget, drivers } from './drivers';

export function useTableRelationships({ target }: { target: DataTarget }) {
  const { data: source, isSuccess: isMetadataReady } = useMetadataSource(
    target.database
  );

  // FIX ME: having fallback for source.kind is a bad idea.
  // we should make the fix on useMetadataSource to either return a valid value or throw an error
  const driver = drivers[source?.kind ?? 'postgres'];

  const query = driver.useTableRelationshipsQuery({
    target,
    queryOptions: {
      enabled: isMetadataReady,
    },
  });

  return query;
}
