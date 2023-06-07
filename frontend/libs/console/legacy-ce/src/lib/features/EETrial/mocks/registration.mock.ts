import { graphql } from 'msw';
import { eeTrialsLuxDataEndpoint } from '../utils';
import { GraphQLError } from 'graphql';
import { EETrialRegistrationResponse } from '../types';

const controlPlaneApi = graphql.link(eeTrialsLuxDataEndpoint);

export const registerEETrialLicenseActiveMutation =
  controlPlaneApi.mutation<EETrialRegistrationResponse>(
    'registerEETrial',
    (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.data({
          registerEETrial: {
            client_id: 'id',
            client_secret: 'secret',
          },
        })
      );
    }
  );

export const registerEETrialErrorMutation = controlPlaneApi.mutation<
  GraphQLError[]
>('registerEETrial', (req, res, ctx) => {
  return res(
    ctx.status(200),
    ctx.errors([
      {
        extensions: {
          code: 'legacyError',
        },
        message: "couldn't find registerEETrial in mutation_root",
      },
    ])
  );
});

export const registerEETrialLicenseAlreadyAppliedMutation =
  controlPlaneApi.mutation<GraphQLError[]>(
    'registerEETrial',
    (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.errors([
          {
            extensions: {
              code: 'legacyError',
              id: '8fae8d3f-e411-4476-b28d-12cfbd715c21',
            },
            message: 'license already applied',
          },
        ])
      );
    }
  );

export const activateEETrialMutatationSuccess =
  controlPlaneApi.mutation<EETrialRegistrationResponse>(
    'registerEETrial',
    (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.data({
          registerEETrial: {
            client_id: 'id',
            client_secret: 'secret',
          },
        })
      );
    }
  );
