import {
  MetadataSelectors,
  useMetadata,
} from '../../../../hasura-metadata-api';
import { adaptFunctionName } from '../utils';

export const useTrackedFunctions = (dataSourceName: string) => {
  return useMetadata(m =>
    (MetadataSelectors.findSource(dataSourceName)(m)?.functions ?? []).map(
      fn => ({
        qualifiedFunction: fn.function,
        name: adaptFunctionName(fn.function).join(' / '),
      })
    )
  );
};
