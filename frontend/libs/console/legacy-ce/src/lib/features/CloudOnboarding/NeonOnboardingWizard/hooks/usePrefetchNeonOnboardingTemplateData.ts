import { useEffect } from 'react';
import { useQueryClient } from '@tanstack/react-query';
import {
  getMetadataUrl,
  getMigrationUrl,
  getSampleQueriesUrl,
  getSchemaImageUrl,
} from '../../constants';
import { fetchTemplateDataQueryFn } from '../utils';

/**
 * Prefetch migrations and metadata file contents for NEON onboarding. Use it to get data early
 * so data is ready by the time we need it. For example this could be fired while Neon
 * DB is being created.
 */
export const usePrefetchNeonOnboardingTemplateData = (
  templateBaseUrl: string,
) => {
  const queryClient = useQueryClient();

  useEffect(() => {
    const metadataUrl = getMetadataUrl(templateBaseUrl);
    queryClient.prefetchQuery({
      queryKey: [metadataUrl],
      queryFn: () => fetchTemplateDataQueryFn(metadataUrl, {}),
    });

    const migrationUrl = getMigrationUrl(templateBaseUrl);
    queryClient.prefetchQuery({
      queryKey: [migrationUrl],
      queryFn: () => fetchTemplateDataQueryFn(migrationUrl, {}),
    });

    const sampleQueriesUrl = getSampleQueriesUrl(templateBaseUrl);
    queryClient.prefetchQuery({
      queryKey: [sampleQueriesUrl],
      queryFn: () => fetchTemplateDataQueryFn(sampleQueriesUrl, {}),
    });

    const schemaImageUrl = getSchemaImageUrl(templateBaseUrl);
    queryClient.prefetchQuery({
      queryKey: [schemaImageUrl],
      queryFn: () => fetchTemplateDataQueryFn(schemaImageUrl, {}),
    });
    // empty deps as it should only run once the component mounts
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);
};
