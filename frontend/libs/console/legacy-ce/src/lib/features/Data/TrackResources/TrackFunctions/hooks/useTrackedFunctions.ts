import { useMetadata } from '@hasura/metadata/api';
import {
  functionDisplayName,
  MetadataSelectors,
} from '@hasura/metadata/helpers';

export const useTrackedFunctions = (dataSourceName: string) => {
  return useMetadata((m) =>
    (MetadataSelectors.findSource(dataSourceName)(m)?.functions ?? []).map(
      (fn) => ({
        qualifiedFunction: fn.function,
        name: functionDisplayName({
          qualifiedFunction: fn.function,
        }),
      }),
    ),
  );
};
