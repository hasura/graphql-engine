import { graphql } from 'msw';
import Endpoints from '@/Endpoints';
import { SurveysResponseData } from '@/features/Surveys';
import { fetchSurveysDataResponse } from './constants';

type ResponseBodyOnSuccess = {
  status: 'success';
};

const controlPlaneApi = graphql.link(Endpoints.luxDataGraphql);

export const baseHandlers = () => [
  controlPlaneApi.query<ResponseBodyOnSuccess>(
    'fetchAllExperimentsData',
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
    'addSurveyAnswer',
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
    'trackExperimentsCohortActivity',
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

export const fetchUnansweredSurveysHandler = controlPlaneApi.query<
  SurveysResponseData['data']
>('fetchAllSurveysData', (req, res, ctx) => {
  return res(ctx.status(200), ctx.data(fetchSurveysDataResponse.unanswered));
});

export const fetchAnsweredSurveysHandler = controlPlaneApi.query<
  SurveysResponseData['data']
>('fetchAllSurveysData', (req, res, ctx) => {
  return res(ctx.status(200), ctx.data(fetchSurveysDataResponse.answered));
});
