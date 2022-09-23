import { graphql } from 'msw';
import Endpoints from '@/Endpoints';

type ResponseBodyOnSuccess = {
  status: 'success';
};

const controlPlaneApi = graphql.link(Endpoints.luxDataGraphql);

export const handlers = () => [
  controlPlaneApi.operation<any, ResponseBodyOnSuccess>((req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.data({
        status: 'success',
      })
    );
  }),
];
