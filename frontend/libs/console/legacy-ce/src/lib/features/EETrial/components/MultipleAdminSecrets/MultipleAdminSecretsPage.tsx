import React from 'react';
import { MultipleAdminSecretsSvg } from './MultipleAdminSecretsSvg';
import { EETrialCard } from '../EETrialCard/EETrialCard';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import globals from '../../../../Globals';

export const MultipleAdminSecretsPage = () => {
  const { access } = useEELiteAccess(globals);
  const isFeatureForbidden = access === 'forbidden';

  const isFeatureActive = access === 'active';

  if (isFeatureForbidden) return null;

  return (
    <div className="flex max-w-screen-md p-md">
      <div className="max-w-3xl">
        <div className="text-xl text-slate-900 font-semibold mb-xs">
          Multiple Admin Secrets
        </div>
        <div className="mt-0 mb-xs">
          <span className="text-muted">
            Enable access to your Hasura instance using multiple
            x-hasura-admin-secrets.
          </span>
          <a
            href="https://hasura.io/docs/latest/security/multiple-admin-secrets/"
            target="_blank"
            rel="noopener noreferrer"
            className="italic font-thin text-sm ml-1 text-secondary"
          >
            (Know More)
          </a>
        </div>
        <MultipleAdminSecretsSvg />
        {isFeatureActive ? (
          <p className="mt-md text-muted">
            <strong>Setup Multiple Admin Secrets</strong>
            <br />
            <a
              className="font-bold text-secondary"
              href="https://hasura.io/docs/latest/security/multiple-admin-secrets/"
            >
              Read more
            </a>{' '}
            on setting up multiple admin secrets for your Hasura instance.
            <br />
            Multiple admin secrets may be enabled by setting the environment
            variable:<code>HASURA_GRAPHQL_ADMIN_SECRETS</code>
          </p>
        ) : (
          <EETrialCard
            className="mt-md"
            id="multiple-admin-secrets"
            cardTitle="Want to enable multiple secrets for your instance?"
            cardText={
              <span>
                Implement security mechanisms like rotating secrets and have
                different lifecycles for individual admin secrets.
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
