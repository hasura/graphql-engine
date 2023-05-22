import { parse, print } from 'graphql';
import { cloudDataServiceApiClient } from '../../../hooks/cloudDataServiceApiClient';
import { Api } from '../../../hooks/apiUtils';
import { HasuraMetadataV3 } from '../../../metadata/types';
import { reactQueryClient } from '../../../lib/reactQuery';
import { programmaticallyTraceError } from '../../Analytics';
import {
  skippedNeonOnboardingVariables,
  onboardingCompleteVariables,
  templateSummaryRunQueryClickVariables,
  templateSummaryRunQuerySkipVariables,
  hasuraSourceCreationStartVariables,
  trackOnboardingActivityMutation,
  onboardingQueryKey,
  fetchAllOnboardingDataQuery,
  fetchAllOnboardingDataQueryVariables,
  oneClickDeploymentOnboardingShown,
  useCaseExperimentOnboarding,
  skippedOnboardingThroughURLParamVariables,
} from './constants';
import { WizardState } from './hooks/useWizardState';
import { OnboardingResponseData, UserOnboarding } from './types';
import { getLSItem, LS_KEYS } from '../../../utils';

export function shouldShowOnboarding(onboardingData: UserOnboarding) {
  const userActivity = onboardingData?.activity;

  if (
    userActivity?.[skippedNeonOnboardingVariables.kind]?.value === 'true' ||
    userActivity?.[skippedOnboardingThroughURLParamVariables.kind]?.value ===
      'true' ||
    userActivity?.[onboardingCompleteVariables.kind]?.value === 'true' ||
    userActivity?.[hasuraSourceCreationStartVariables.kind]?.value === 'true' ||
    userActivity?.[templateSummaryRunQuerySkipVariables.kind]?.value ===
      'true' ||
    userActivity?.[templateSummaryRunQueryClickVariables.kind]?.value ===
      'true' ||
    userActivity?.[oneClickDeploymentOnboardingShown.kind]?.value === 'true' ||
    userActivity?.[useCaseExperimentOnboarding.kind]?.value === 'true' ||
    userActivity?.[oneClickDeploymentOnboardingShown.kind]?.value === 'true'
  ) {
    return false;
  }
  if (getLSItem(LS_KEYS.skipOnboarding) === 'true') {
    emitOnboardingEvent(skippedOnboardingThroughURLParamVariables);
    return false;
  }
  return true;
}

const nullUserOnboardingData = {
  activity: {},
  target: 'cloud_console',
};

/**
 * Transforms server returned data to the required format.
 */
function onboardingDataTransformFn(
  data: OnboardingResponseData
): UserOnboarding {
  return (
    data?.data?.user_onboarding?.find(
      onboarding => onboarding.target === 'cloud_console'
    ) || nullUserOnboardingData
  );
}

export function getWizardState(
  showFamiliaritySurvey: boolean,
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

  // transform the onboarding data if present, to a consumable format
  const transformedOnboardingData = onboardingDataTransformFn(onboardingData);
  if (shouldShowOnboarding(transformedOnboardingData)) {
    if (getLSItem(LS_KEYS.useCaseExperimentOnboarding)) {
      return 'use-case-onboarding';
    }
    if (showFamiliaritySurvey) return 'familiarity-survey';
    return 'landing-page';
  }
  return 'hidden';
}

type ResponseDataOnMutation = {
  data: {
    trackOnboardingActivity: {
      status: string;
    };
  };
};

const cloudHeaders = {
  'content-type': 'application/json',
};

export const emitOnboardingEvent = (variables: Record<string, unknown>) => {
  // mutate server data
  cloudDataServiceApiClient<ResponseDataOnMutation, ResponseDataOnMutation>(
    trackOnboardingActivityMutation,
    variables,
    cloudHeaders
  ).catch(error => {
    programmaticallyTraceError(error);
  });
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

/**
 * Utility function to parse a string with multiple graphql queries and pick the one with given operation name
 * */
export function getQueryFromSampleQueries(
  allQueries: string,
  operationName: string
): string {
  const doc = parse(allQueries);
  const { definitions: allDefinitions } = doc;
  const definitions = allDefinitions.filter(
    d => d.kind === 'OperationDefinition'
  );
  let queryDef = definitions
    .filter(d => d.kind === 'OperationDefinition')
    .find(d => {
      if (d.kind === 'OperationDefinition') {
        return d.name?.value === operationName;
      }
      return false;
    });
  if (!queryDef) {
    if (definitions.length > 0) {
      queryDef = definitions[0];
    } else {
      throw new Error('no valid operations in sample.graphql');
    }
  }

  return print({
    ...doc,
    definitions: [queryDef],
  });
}

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
