import { cloudDataServiceApiClient } from '../../../hooks/cloudDataServiceApiClient';
import {
  fetchAllOnboardingDataQuery,
  fetchAllOnboardingDataQueryVariables,
} from '../constants';
import { WizardState } from './hooks/useWizardState';
import { OnboardingResponseData } from '../types';
import { HasuraMetadataV3, HttpError } from '@hasura/shared/types';
import { request } from '@hasura/shared/utils';

export function getWizardState(
  onboardingData?: OnboardingResponseData,
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
 *
 * Unlike the generic `requestJson` helper, this reads the response body based on its
 * `Content-Type`: JSON responses are parsed, everything else (e.g. the raw SQL migration
 * files, or metadata/sample-query files served as `text/plain`) is returned as text. This
 * matters here because some of these template files contain JSON-encoded text that the
 * caller (e.g. `useInstallMetadata`) expects to parse itself.
 */
export async function fetchTemplateDataQueryFn<
  ResponseData,
  TransformedData = ResponseData,
>(
  dataUrl: string,
  headers: Record<string, string>,
  transformFn?: (data: ResponseData) => TransformedData,
) {
  const response = await request(dataUrl, {
    method: 'GET',
    headers,
  });

  const contentType = response.headers.get('content-type');
  const isJsonResponse = !!contentType?.includes('application/json');

  const data = (
    isJsonResponse ? await response.json() : await response.text()
  ) as ResponseData;

  return transformFn ? transformFn(data) : data;
}

/**
 * Builds a human (and test) friendly message out of an error thrown by the shared
 * `request`/`requestJson` helpers. `HttpError`'s own `message` is just the HTTP
 * status text (e.g. "Service Unavailable"); the actual server-provided error body
 * lives in `error.data`. Prefer surfacing that body (stringified, since callers of
 * this hook expect a single string) when present, falling back to `error.message`
 * for network errors or non-HTTP failures where there is no response body.
 */
export const getHttpErrorMessage = (error: Error): string => {
  if (error instanceof HttpError && error.data !== undefined) {
    return typeof error.data === 'string'
      ? error.data
      : JSON.stringify(error.data);
  }
  return error.message;
};

/**
 * Utility function which merges the old and additional metadata objects
 * to create a new metadata object and returns it.
 */
export const transformOldMetadata = (
  oldMetadata: HasuraMetadataV3,
  additionalMetadata: HasuraMetadataV3,
  source: string,
) => {
  const newMetadata: HasuraMetadataV3 = {
    ...oldMetadata,
    sources:
      oldMetadata?.sources?.map((oldSource) => {
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
    cloudHeaders,
  );
