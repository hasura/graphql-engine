import React from 'react';
import globals from '../../../../Globals';
import { ApiSecuritySvg } from './ApiSecuritySvg';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import { EETrialCard } from '../EETrialCard/EETrialCard';

type Props = {
  children?: React.ReactElement;
};

// This tab shows an example component of how the EE registration button and hooks for fetching
// license info can be used to build a promotional component for EE, behind which the actual
// feature can live.
//
// This component has a check for pro-lite and license status. If the license is not active we show the component
// specific EE promotion UI. And use the Enable Enterprise button wrapper to start the registration flow.
export function ApiSecurityTabEELiteWrapper(props: Props) {
  const { children } = props;
  const { access } = useEELiteAccess(globals);

  if (
    globals.consoleType === 'cloud' ||
    globals.consoleType === 'pro' ||
    access === 'active'
  ) {
    return children ?? null;
  }

  if (access === 'forbidden') {
    return null;
  }

  return (
    <div className="flex justify-center">
      <div className="max-w-3xl">
        <div className="text-xl text-slate-900 font-semibold mb-xs">
          API Security
        </div>
        <div className="mt-0 mb-xs">
          <span className="text-muted">
            Enable advanced security options to help secure your GraphQL API for
            production.
          </span>
          <a
            href="https://hasura.io/docs/latest/security/index"
            target="_blank"
            rel="noopener noreferrer"
            className="italic font-thin text-sm ml-1 text-secondary"
          >
            (Know More)
          </a>
        </div>
        <ApiSecuritySvg />
        <EETrialCard
          id="security-tab"
          className="mt-md"
          cardTitle="Production grade security for your API"
          cardText={
            <span>
              Add additional security features to your API such as depth / node
              limits, rate limiting (RPM), batch requests limits, timeouts, and
              schema introspection.
            </span>
          }
          buttonLabel="Enable Enterprise"
          eeAccess={access}
          horizontal
        />
      </div>
    </div>
  );
}
