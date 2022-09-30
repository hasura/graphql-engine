import { parse, print } from 'graphql';
import { ExperimentConfig } from '@/features/GrowthExperiments';
import { Dispatch } from '@/types';
import { cloudDataServiceApiClient } from '@/hooks/cloudDataServiceApiClient';
import { Api } from '@/hooks/apiUtils';
import { HasuraMetadataV3 } from '@/metadata/types';
import {
  clickRunQueryButton,
  forceGraphiQLIntrospection,
  forceChangeGraphiqlQuery,
} from '../../components/Services/ApiExplorer/OneGraphExplorer/utils';
import {
  skippedOnboardingVariables,
  onboardingCompleteVariables,
  templateSummaryRunQueryClickVariables,
  templateSummaryRunQuerySkipVariables,
  hasuraSourceCreationStartVariables,
  graphQlMutation,
} from './constants';

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
  experimentId: string,
  hasNeonAccess: boolean
) {
  const experimentData = experimentsData?.find(
    experimentConfig => experimentConfig.experiment === experimentId
  );

  const userActivity = experimentData?.userActivity;

  // onboarding skipped/completion is different with the new Neon flow
  if (hasNeonAccess) {
    if (
      userActivity?.[skippedOnboardingVariables.kind] ||
      userActivity?.[onboardingCompleteVariables.kind] ||
      userActivity?.[hasuraSourceCreationStartVariables.kind] ||
      userActivity?.[templateSummaryRunQuerySkipVariables.kind] ||
      userActivity?.[templateSummaryRunQueryClickVariables.kind]
    ) {
      return false;
    }
    return true;
  }

  if (userActivity?.onboarding_complete || userActivity?.skipped_onboarding) {
    return false;
  }
  return true;
}

type ResponseDataOnMutation = {
  data: {
    trackExperimentsCohortActivity: {
      status: string;
    };
  };
};

const cloudHeaders = {
  'content-type': 'application/json',
};

// persist skipped onboarding in the database
export const persistSkippedOnboarding = () => {
  // mutate server data
  cloudDataServiceApiClient<ResponseDataOnMutation, ResponseDataOnMutation>(
    graphQlMutation,
    skippedOnboardingVariables,
    cloudHeaders
  ).catch(error => {
    // TODO throw Sentry alert
    throw error;
  });
};

// persist onboarding completion in the database
export const persistOnboardingCompletion = () => {
  // mutate server data
  cloudDataServiceApiClient<ResponseDataOnMutation, ResponseDataOnMutation>(
    graphQlMutation,
    onboardingCompleteVariables,
    cloudHeaders
  ).catch(error => {
    // TODO throw Sentry alert
    console.error(error);
  });
};

export const emitOnboardingEvent = (variables: Record<string, unknown>) => {
  // mutate server data
  cloudDataServiceApiClient<ResponseDataOnMutation, ResponseDataOnMutation>(
    graphQlMutation,
    variables,
    cloudHeaders
  ).catch(error => {
    // TODO throw Sentry alert
    console.error(error);
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

export const runQueryInGraphiQL = () => {
  clickRunQueryButton();
};

export const fillSampleQueryInGraphiQL = (
  query: string,
  dispatch: Dispatch
) => {
  forceGraphiQLIntrospection(dispatch);

  // this timeout makes sure that there's a delay in setting query after introspection has been fired
  // this timeout does not intend to wait for introspection to finish
  setTimeout(() => {
    forceChangeGraphiqlQuery(query, dispatch);
  }, 500);
};
