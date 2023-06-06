import { graphql, rest } from 'msw';
import Endpoints from '../../../../Endpoints';
import {
  fetchSurveysDataResponse,
  mockMetadataUrl,
  mockMigrationUrl,
  mockOnboardingData,
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
  controlPlaneApi.mutation<ResponseBodyOnSuccess>(
    'addSurveyAnswerV2',
    (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.data({
          status: 'success',
        })
      );
    }
  ),
  controlPlaneApi.mutation<ResponseBodyOnSuccess>(
    'trackOnboardingActivity',
    (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.data({
          status: 'success',
        })
      );
    }
  ),
];

export const fetchOnboardingDataFailure = controlPlaneApi.query<
  Record<string, string>
>('fetchAllOnboardingData', (req, res, ctx) => {
  return res(ctx.status(503), ctx.data(serverDownErrorMessage));
});

export const onboardingDataEmptyActivity = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', (req, res, ctx) => {
  return res(ctx.status(200), ctx.data(mockOnboardingData.emptyActivity));
});

export const onboardingDataSkippedOnboarding = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', (req, res, ctx) => {
  return res(ctx.status(200), ctx.data(mockOnboardingData.skippedOnboarding));
});

export const onboardingDataCompleteOnboarding = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', (req, res, ctx) => {
  return res(ctx.status(200), ctx.data(mockOnboardingData.completedOnboarding));
});

export const onboardingDataHasuraSourceCreationStart = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', (req, res, ctx) => {
  return res(
    ctx.status(200),
    ctx.data(mockOnboardingData.hasuraDataSourceCreationStart)
  );
});

export const onboardingDataRunQueryClick = controlPlaneApi.query<
  OnboardingResponseData['data']
>('fetchAllOnboardingData', (req, res, ctx) => {
  return res(ctx.status(200), ctx.data(mockOnboardingData.runQueryClick));
});

export const fetchUnansweredSurveysHandler =
  controlPlaneApi.query<FetchAllSurveysDataQuery>(
    'fetchAllSurveysData',
    (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.data(fetchSurveysDataResponse.unanswered)
      );
    }
  );

export const fetchAnsweredSurveysHandler =
  controlPlaneApi.query<FetchAllSurveysDataQuery>(
    'fetchAllSurveysData',
    (req, res, ctx) => {
      return res(ctx.status(200), ctx.data(fetchSurveysDataResponse.answered));
    }
  );

export const fetchGithubMetadataHandler = rest.get(
  mockMetadataUrl,
  (req, res, ctx) => {
    return res(ctx.text(JSON.stringify(MOCK_METADATA_FILE_CONTENTS)));
  }
);

export const fetchGithubMigrationHandler = rest.get(
  mockMigrationUrl,
  (req, res, ctx) => {
    return res(ctx.text(MOCK_MIGRATION_FILE_CONTENTS));
  }
);

export const mockGithubServerDownHandler = (url: string) =>
  rest.get(url, (req, res, ctx) => {
    return res(ctx.status(503), ctx.json(serverDownErrorMessage));
  });

export const metadataSuccessHandler = rest.post(
  Endpoints.metadata,
  async (req, res, ctx) => {
    const body = (await req.json()) as Record<string, unknown>;

    if (body.type === 'replace_metadata' || body.type === 'reload_metadata') {
      return res(ctx.json({ message: 'success' }));
    }

    return res(
      ctx.status(400),
      ctx.json({
        code: 'parse-failed',
        error: `unknown metadata command ${body.type}`,
        path: '$',
      })
    );
  }
);

export const metadataFailureHandler = rest.post(
  Endpoints.metadata,
  async (req, res, ctx) => {
    return res(ctx.status(503), ctx.json(serverDownErrorMessage));
  }
);

export const querySuccessHandler = rest.post(
  Endpoints.queryV2,
  async (req, res, ctx) => {
    const body = (await req.json()) as Record<string, unknown>;

    if (body.type === 'run_sql') {
      return res(ctx.json({ message: 'success' }));
    }

    return res(
      ctx.status(400),
      ctx.json({
        code: 'parse-failed',
        error: `unknown metadata command ${body.type}`,
        path: '$',
      })
    );
  }
);

export const queryFailureHandler = rest.post(
  Endpoints.queryV2,
  async (req, res, ctx) => {
    return res(ctx.status(503), ctx.json(serverDownErrorMessage));
  }
);
