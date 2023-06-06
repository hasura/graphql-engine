import { cloudDataServiceApiClient } from '../../../hooks/cloudDataServiceApiClient';
import { Api } from '../../../hooks/apiUtils';
import { HasuraMetadataV3 } from '../../../metadata/types';
import { reactQueryClient } from '../../../lib/reactQuery';
import {
  fetchAllOnboardingDataQuery,
  fetchAllOnboardingDataQueryVariables,
  onboardingQueryKey,
} from '../constants';
import { WizardState } from './hooks/useWizardState';
import { OnboardingResponseData } from '../types';

export function getWizardState(
  onboardingData?: OnboardingResponseData
): WizardState {
  // if onbarding data is not present due to api error, or data loading state, then hide the wizard
  // this early return is required to distinguish between server errors vs data not being present for user
  // if the request is successful and data is not present for the given user, then we should show the onboarding wizard
  if (!onboardingData?.data) return 'hidden';

  // if user created account before the launch of onboarding wizard (Oct 17, 2022),
  // hide the wizard and survey
  const userCreatedAt = new Date(onboardingData.data.users[0].created_at);
  if (userCreatedAt.getTime() < 1666008600000) {
    return 'hidden';
  }

  if (!onboardingData.data.user_onboarding[0]?.is_onboarded) {
    return 'landing-page';
  }
  return 'hidden';
}

const cloudHeaders = {
  'content-type': 'application/json',
};

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

export const fetchAllOnboardingDataQueryFn = () =>
  cloudDataServiceApiClient<OnboardingResponseData, OnboardingResponseData>(
    fetchAllOnboardingDataQuery,
    fetchAllOnboardingDataQueryVariables,
    cloudHeaders
  );

export const prefetchOnboardingData = () => {
  reactQueryClient.prefetchQuery(
    onboardingQueryKey,
    fetchAllOnboardingDataQueryFn
  );
};
