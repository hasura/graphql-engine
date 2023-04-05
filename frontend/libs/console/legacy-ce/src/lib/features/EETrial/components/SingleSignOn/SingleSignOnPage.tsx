import React from 'react';
import { SingleSignOnSvg } from './SingleSignOnSvg';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import { EETrialCard } from '../EETrialCard/EETrialCard';
import globals from '../../../../Globals';

export const SingleSignOnPage = () => {
  const { access } = useEELiteAccess(globals);

  return (
    <div className="flex max-w-screen-md p-md">
      <div className="max-w-3xl">
        <div className="text-xl text-slate-900 font-semibold mb-xs">
          Single Sign On (SSO)
        </div>
        <div className="mt-0 mb-xs">
          <span className="text-muted">
            Enable secure organization access to manage your Hasura instance by
            integrating with single sign-on (SSO)
          </span>
          <a
            href="https://hasura.io/docs/latest/hasura-cloud/sso/"
            target="_blank"
            rel="noopener noreferrer"
            className="italic font-thin text-sm ml-1 text-secondary"
          >
            (Know More)
          </a>
        </div>
        <SingleSignOnSvg />
        {access === 'active' ||
        globals.consoleType === 'cloud' ||
        globals.consoleType === 'pro' ? (
          <p className="mt-md text-muted">
            <strong>Setup Single Sign-On (SSO)</strong>
            <br />
            <a
              className="font-bold text-secondary"
              href="https://hasura.io/docs/latest/hasura-cloud/sso/"
            >
              Read more
            </a>{' '}
            on setting up multiple single sign-on (SSO) for your Hasura instance
            and your organization.
          </p>
        ) : (
          <EETrialCard
            className="mt-md"
            id="sso"
            cardTitle="Looking to secure your instance with single sign-on?"
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
