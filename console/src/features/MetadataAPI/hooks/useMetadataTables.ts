import { useAppSelector } from '@/store';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useMetadataTables = () => {
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  return useMetadata(MetadataSelector.getTables(source));
};
