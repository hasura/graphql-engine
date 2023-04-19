import React from 'react';
import { MultipleJWTSecretsSvg } from './MultipleJWTSecretsSvg';
import { EETrialCard } from '../EETrialCard/EETrialCard';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import globals from '../../../../Globals';

export const MultipleJWTSecretsPage = () => {
  const { access } = useEELiteAccess(globals);
  const isFeatureForbidden = access === 'forbidden';

  const isFeatureActive = access === 'active';

  if (isFeatureForbidden) return null;

  return (
    <div className="flex max-w-screen-md p-md">
      <div className="max-w-3xl">
        <div className="text-xl text-slate-900 font-semibold mb-xs">
          Multiple JWT Secrets
        </div>
        <div className="mt-0 mb-xs">
          <span className="text-muted">
            Enable access to your Hasura instance using multiple JSON web token
            secrets
          </span>
          <a
            href="https://hasura.io/docs/latest/security/multiple-jwt-secrets/"
            target="_blank"
            rel="noopener noreferrer"
            className="italic font-thin text-sm ml-1 text-secondary"
          >
            (Know More)
          </a>
        </div>
        <MultipleJWTSecretsSvg />
        {isFeatureActive ? (
          <p className="mt-md text-muted">
            <strong>Setup Multiple JWT Secrets</strong>
            <br />
            <a
              className="font-bold text-secondary"
              href="https://hasura.io/docs/latest/security/multiple-jwt-secrets/"
            >
              Read more
            </a>{' '}
            on setting up multiple JWT secrets for your Hasura instance.
            <br />
            Multiple admin secrets may be enabled by setting the environment
            variable: <code>HASURA_GRAPHQL_JWT_SECRETS</code>
          </p>
        ) : (
          <EETrialCard
            id="multiple-jwt-secrets"
            className="mt-md"
            cardTitle="Want to enable multiple secrets for your instance?"
            cardText={
              <span>
                Get production-ready today with a 30-day free trial of Hasura
                EE, no credit card required.
              </span>
            }
            buttonLabel="Enable Enterprise"
            eeAccess={access}
            horizontal
          />
        )}
      </div>
    </div>
  );
};
