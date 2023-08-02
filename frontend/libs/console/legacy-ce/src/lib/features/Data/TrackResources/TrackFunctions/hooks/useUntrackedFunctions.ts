import { Feature, IntrospectedFunction } from '../../../../DataSource';
import {
  MetadataSelectors,
  areTablesEqual,
  useMetadata,
} from '../../../../hasura-metadata-api';
import { MetadataFunction } from '../../../../hasura-metadata-types';
import { useIntrospectedFunctions } from '../../../hooks/useTrackableFunctions';
import { adaptFunctionName } from '../utils';

export const adaptUntrackedFunctions =
  (metadataFunctions: MetadataFunction[]) =>
  (introspectedFunctions: IntrospectedFunction[] | Feature) => {
    if (introspectedFunctions === Feature.NotImplemented)
      return introspectedFunctions;

    const trackedFunctions = metadataFunctions.map(fn =>
      adaptFunctionName(fn.function)
    );

    return introspectedFunctions.filter(fn => {
      const isAlreadyTracked = trackedFunctions.find(trackedFn =>
        areTablesEqual(fn.qualifiedFunction, trackedFn)
      );
      return !isAlreadyTracked;
    });
  };

export const useUntrackedFunctions = (dataSourceName: string) => {
  const { data: trackedFunctions = [], isFetched } = useMetadata(
    m => MetadataSelectors.findSource(dataSourceName)(m)?.functions ?? []
  );

  return useIntrospectedFunctions({
    dataSourceName,
    options: {
      select: introspectedFunctions =>
        adaptUntrackedFunctions(trackedFunctions)(introspectedFunctions),
      enabled: isFetched,
    },
  });
};
