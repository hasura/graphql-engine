import { HttpResponse } from 'msw';
import { graphql } from 'msw/graphql';
import { eeTrialsLuxDataEndpoint } from '../utils';
import { EETrialRegistrationResponse } from '../types';

const controlPlaneApi = graphql.link(eeTrialsLuxDataEndpoint);

export const registerEETrialLicenseActiveMutation =
  controlPlaneApi.mutation<EETrialRegistrationResponse>(
    'registerEETrial',
    () => {
      return HttpResponse.json(
        {
          data: {
            registerEETrial: {
              client_id: 'id',
              client_secret: 'secret',
            },
          },
        },
        { status: 200 },
      );
    },
  );

export const registerEETrialErrorMutation = controlPlaneApi.mutation(
  'registerEETrial',
  () => {
    return HttpResponse.json(
      {
        errors: [
          {
            extensions: {
              code: 'legacyError',
            },
            message: "couldn't find registerEETrial in mutation_root",
          },
        ],
      },
      { status: 200 },
    );
  },
);

export const registerEETrialLicenseAlreadyAppliedMutation =
  controlPlaneApi.mutation('registerEETrial', () => {
    return HttpResponse.json(
      {
        errors: [
          {
            extensions: {
              code: 'legacyError',
              id: '8fae8d3f-e411-4476-b28d-12cfbd715c21',
            },
            message: 'license already applied',
          },
        ],
      },
      { status: 200 },
    );
  });

export const activateEETrialMutatationSuccess =
  controlPlaneApi.mutation<EETrialRegistrationResponse>(
    'registerEETrial',
    () => {
      return HttpResponse.json(
        {
          data: {
            registerEETrial: {
              client_id: 'id',
              client_secret: 'secret',
            },
          },
        },
        { status: 200 },
      );
    },
  );
