import { ExperimentConfig } from '@/features/GrowthExperiments';
import { Api } from '@/hooks/apiUtils';
import { HasuraMetadataV3 } from '@/metadata/types';

export function isExperimentActive(
  experimentsData: ExperimentConfig[],
  experimentId: string
) {
  const experimentData = experimentsData?.find(
    experimentConfig => experimentConfig.experiment === experimentId
  );
  return experimentData && experimentData?.status === 'enabled';
}

export function shouldShowOnboarding(
  experimentsData: ExperimentConfig[],
  experimentId: string
) {
  const experimentData = experimentsData?.find(
    experimentConfig => experimentConfig.experiment === experimentId
  );
  if (
    experimentData?.userActivity?.onboarding_complete ||
    experimentData?.userActivity?.skipped_onboarding
  ) {
    return false;
  }
  return true;
}

/**
 * Utility function to be used as a react query QueryFn, which does a `GET` request to
 * fetch our requested object, and returns a promise.
 */
export function fetchTemplateDataQueryFn<
  ResponseData,
  TransformedData = ResponseData
>(
  dataUrl: string,
  headers: Record<string, string>,
  transformFn?: (data: ResponseData) => TransformedData
) {
  return Api.get<ResponseData, TransformedData>(
    {
      url: dataUrl,
      headers,
    },
    transformFn
  );
}

/**
 * Utility function which merges the old and additional metadata objects
 * to create a new metadata object and returns it.
 */
export const transformOldMetadata = (
  oldMetadata: HasuraMetadataV3,
  additionalMetadata: HasuraMetadataV3,
  source: string
) => {
  const newMetadata: HasuraMetadataV3 = {
    ...oldMetadata,
    sources:
      oldMetadata?.sources?.map(oldSource => {
        if (oldSource.name !== source) {
          return oldSource;
        }
        const metadataObject = additionalMetadata?.sources?.[0];
        if (!metadataObject) {
          return oldSource;
        }
        return {
          ...oldSource,
          tables: [...oldSource.tables, ...(metadataObject.tables ?? [])],
          functions: [
            ...(oldSource.functions ?? []),
            ...(metadataObject.functions ?? []),
          ],
        };
      }) ?? [],
  };
  return newMetadata;
};
