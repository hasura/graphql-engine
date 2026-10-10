import React from 'react';
import { ApiSecuritySvg } from './ApiSecuritySvg';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import { EETrialCard } from '../EETrialCard/EETrialCard';
import { Flex, Heading, Link, Text } from '@radix-ui/themes';

type Props = {
  children?: React.ReactElement<any>;
};

// This tab shows an example component of how the EE registration button and hooks for fetching
// license info can be used to build a promotional component for EE, behind which the actual
// feature can live.
//
// This component has a check for pro-lite and license status. If the license is not active we show the component
// specific EE promotion UI. And use the Enable Enterprise button wrapper to start the registration flow.
export function ApiSecurityTabEELiteWrapper(props: Props) {
  const { children } = props;
  const { access, consoleType } = useEELiteAccess();

  if (consoleType === 'cloud' || consoleType === 'pro' || access === 'active') {
    return children ?? null;
  }

  if (access === 'forbidden') {
    return null;
  }

  return (
    <Flex justify="center" direction="column" gap="4" className="w-8/12">
      <Heading size="4">API Security</Heading>
      <div>
        <Text size="2">
          Enable advanced security options to help secure your GraphQL API for
          production.
        </Text>{' '}
        <Link
          href="https://hasura.io/docs/latest/security/index"
          target="_blank"
          rel="noopener noreferrer"
          size="1"
          className="italic"
        >
          (Know More)
        </Link>
      </div>
      <ApiSecuritySvg className="w-full" />
      <EETrialCard
        id="security-tab"
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
    </Flex>
  );
}
