import { http, HttpResponse } from 'msw';
import { graphql } from 'msw/graphql';
import Endpoints from '../../../../Endpoints';
import {
  fetchSurveysDataResponse,
  mockMetadataUrl,
  mockMigrationUrl,
  mockOnboardingData,
  MOCK_INITIAL_METADATA,
  MOCK_METADATA_FILE_CONTENTS,
  MOCK_MIGRATION_FILE_CONTENTS,
  serverDownErrorMessage,
} from './constants';
import { OnboardingResponseData } from '../../types';
import { FetchAllSurveysDataQuery } from '../../../ControlPlane';

type ResponseBodyOnSuccess = {
  status: 'success';
};

const controlPlaneApi = graphql.link(Endpoints.luxDataGraphql);

export const mutationBaseHandlers = () => [
  controlPlaneApi.mutation<ResponseBodyOnSuccess>('addSurveyAnswerV2', () => {
    return HttpResponse.json(
      {
        data: {
          status: 'success',
        },
      },
      { status: 200 },
    );
  }),
  controlPlaneApi.mutation<ResponseBodyOnSuccess>(
    'trackOnboardingActivity',
    () => {
      return HttpResponse.json(
        {
          data: {
            status: 'success',
          },
        },
        { status: 200 },
      );
    },
  ),
];

export const fetchOnboardingDataFailure = controlPlaneApi.query<
  Record<string, string>
>('fetchAllOnboardingData', () => {
  return HttpResponse.json({ data: serverDownErrorMessage }, { status: 503 });
});

export const onboardingDataEmptyActivity = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', () => {
  return HttpResponse.json(
    { data: mockOnboardingData.emptyActivity },
    { status: 200 },
  );
});

export const onboardingDataSkippedOnboarding = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', () => {
  return HttpResponse.json(
    { data: mockOnboardingData.skippedOnboarding },
    { status: 200 },
  );
});

export const onboardingDataCompleteOnboarding = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', () => {
  return HttpResponse.json(
    { data: mockOnboardingData.completedOnboarding },
    { status: 200 },
  );
});

export const onboardingDataHasuraSourceCreationStart = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', () => {
  return HttpResponse.json(
    { data: mockOnboardingData.hasuraDataSourceCreationStart },
    { status: 200 },
  );
});

export const onboardingDataRunQueryClick = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', () => {
  return HttpResponse.json(
    { data: mockOnboardingData.runQueryClick },
    { status: 200 },
  );
});

export const fetchUnansweredSurveysHandler =
  controlPlaneApi.query<FetchAllSurveysDataQuery>('fetchAllSurveysData', () => {
    return HttpResponse.json(
      { data: fetchSurveysDataResponse.unanswered },
      { status: 200 },
    );
  });

export const fetchAnsweredSurveysHandler =
  controlPlaneApi.query<FetchAllSurveysDataQuery>('fetchAllSurveysData', () => {
    return HttpResponse.json(
      { data: fetchSurveysDataResponse.answered },
      { status: 200 },
    );
  });

export const fetchGithubMetadataHandler = http.get(mockMetadataUrl, () => {
  return HttpResponse.text(JSON.stringify(MOCK_METADATA_FILE_CONTENTS));
});

export const fetchGithubMigrationHandler = http.get(mockMigrationUrl, () => {
  return HttpResponse.text(MOCK_MIGRATION_FILE_CONTENTS);
});

export const mockGithubServerDownHandler = (url: string) =>
  http.get(url, () => {
    return HttpResponse.json(serverDownErrorMessage, { status: 503 });
  });

// `useMetadata()` (used internally by `useInstallMetadata`) always issues an
// `export_metadata` request to fetch the current/old metadata before a
// `replace_metadata`/`reload_metadata` call can be made. Both the success and
// failure handlers below need to answer it so that flow can proceed far enough
// to exercise the `replace_metadata` success/failure paths under test.
export const metadataSuccessHandler = http.post(
  Endpoints.metadata,
  async ({ request }) => {
    // read a clone so the original request body stream remains readable for
    // other consumers (e.g. test assertions listening on MSW's request events)
    const body = (await request.clone().json()) as Record<string, unknown>;

    if (body.type === 'export_metadata') {
      return HttpResponse.json({
        resource_version: 1,
        metadata: MOCK_INITIAL_METADATA,
      });
    }

    if (body.type === 'replace_metadata' || body.type === 'reload_metadata') {
      return HttpResponse.json({ message: 'success' });
    }

    return HttpResponse.json(
      {
        code: 'parse-failed',
        error: `unknown metadata command ${body.type}`,
        path: '$',
      },
      { status: 400 },
    );
  },
);

export const metadataFailureHandler = http.post(
  Endpoints.metadata,
  async ({ request }) => {
    const body = (await request.clone().json()) as Record<string, unknown>;

    if (body.type === 'export_metadata') {
      return HttpResponse.json({
        resource_version: 1,
        metadata: MOCK_INITIAL_METADATA,
      });
    }

    return HttpResponse.json(serverDownErrorMessage, { status: 503 });
  },
);

export const querySuccessHandler = http.post(
  Endpoints.queryV2,
  async ({ request }) => {
    const body = (await request.clone().json()) as Record<string, unknown>;

    // `useRunSQLCommand`/`getRunSqlQuery` prefixes the command by driver kind
    // (e.g. `pg_run_sql` for postgres), it never sends a bare `run_sql`.
    if (body.type === 'pg_run_sql') {
      return HttpResponse.json({ message: 'success' });
    }

    return HttpResponse.json(
      {
        code: 'parse-failed',
        error: `unknown metadata command ${body.type}`,
        path: '$',
      },
      { status: 400 },
    );
  },
);

export const queryFailureHandler = http.post(Endpoints.queryV2, () => {
  return HttpResponse.json(serverDownErrorMessage, { status: 503 });
});
