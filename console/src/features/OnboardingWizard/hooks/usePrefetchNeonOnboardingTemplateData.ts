import { useEffect } from 'react';
import { useQueryClient } from 'react-query';
import {
  NEON_METADATA_PATH,
  NEON_MIGRATIONS_PATH,
  NEON_IMAGE_PATH,
  NEON_QUERY_PATH,
} from './constants';
import { fetchTemplateDataQueryFn } from './utils';

/**
 * Prefetch migrations and metadata file contents for NEON onboarding. Use it to get data early
 * so data is ready by the time we need it. For example this could be fired while Neon
 * DB is being created.
 */
export const usePrefetchNeonOnboardingTemplateData = () => {
  const queryClient = useQueryClient();

  useEffect(() => {
    queryClient.prefetchQuery(NEON_MIGRATIONS_PATH, () =>
      fetchTemplateDataQueryFn(NEON_MIGRATIONS_PATH, {})
    );
    queryClient.prefetchQuery(NEON_METADATA_PATH, () =>
      fetchTemplateDataQueryFn(NEON_METADATA_PATH, {})
    );
    queryClient.prefetchQuery(NEON_IMAGE_PATH, () =>
      fetchTemplateDataQueryFn(NEON_IMAGE_PATH, {})
    );
    queryClient.prefetchQuery(NEON_QUERY_PATH, () =>
      fetchTemplateDataQueryFn(NEON_QUERY_PATH, {})
    );
  }, []);
};
