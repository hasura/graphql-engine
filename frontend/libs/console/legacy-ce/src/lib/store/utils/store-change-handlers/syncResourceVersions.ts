import { MetadataHelpers } from '../../../features/hasura-metadata-api/metadataHelpers';
import { logMetadataInvalidation } from '../../../features/hasura-metadata-api/useInvalidateMetadata';
import { OnStoreChangeProps } from '../onStoreChange';

export const syncResourceVersions = ({
  currentStore,
  previousStore,
}: OnStoreChangeProps) => {
  if (previousStore.metadata?.metadataObject == null) {
    // if this is true, then no actual metadata change occurred.
    // metadata just changed from default state to initial actual metadata
    return;
  }

  const redux = {
    previousResourceVersion: previousStore?.metadata?.resourceVersion,
    resourceVersion: currentStore?.metadata?.resourceVersion,
  };
  const reactQueryResourceVersion =
    MetadataHelpers.getQueryData()?.resource_version ?? 0;

  if (
    redux.resourceVersion > redux.previousResourceVersion &&
    redux.resourceVersion > reactQueryResourceVersion
  ) {
    // log the invalidation:
    logMetadataInvalidation({
      componentName: 'syncResourceVersions() in reduxStoreListener()',
      reasons: [
        `Inconsistent resourse version detected between redux and react-query after redux changes:`,
        `previous redux version: ${redux.previousResourceVersion}`,
        `current redux version: ${redux.resourceVersion}`,
        `current react-query version: ${reactQueryResourceVersion}`,
      ],
    });

    // invalidate react-query's metadata key:
    MetadataHelpers.invalidate();
  }
};
