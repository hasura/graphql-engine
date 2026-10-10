import React, { useEffect, useState } from 'react';
import { AdminSecretLoginForm } from '@hasura/console-legacy-ce';
import { getFromLS, initLS } from '../../shared/auth/localStorage';
import { sso3rdPartyEnabled } from '../../shared/auth/utils';
import SSOLoginButton from './SSOLoginButton';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { Flex } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';
import { Button } from '@hasura/shared/ui';
import LoginWithHasura from './LoginWithHasura';
import { FaKey } from 'react-icons/fa6';
import { AdminSecretLogin } from './AdminSecretLogin';

const LoginEE = () => {
  useDocumentTitle('Login | ' + 'Hasura');

  const { envVars } = useAppContext();
  const [showAdminSecretLogin, setShowAdminSecretLogin] = useState(false);

  useEffect(() => {
    const currentState = getFromLS();
    if (!currentState) {
      initLS();
    }
  }, []);

  const doAdminSecretLogin = () => {
    setShowAdminSecretLogin(true);
  };

  const backToLoginHome = () => {
    setShowAdminSecretLogin(false);
  };

  const sso3rdEnabled = sso3rdPartyEnabled(envVars);

  if (!envVars.ssoEnabled && !sso3rdEnabled && !envVars.consoleId) {
    return <AdminSecretLoginForm />;
  }

  const renderLoginMethods = () => {
    const loginMethods: React.ReactNode[] = [];

    // render SSO login buttons from 3rd-party identity providers
    // that EE users define
    if (
      envVars.ssoEnabled !== 'false' &&
      envVars.consoleMode === 'server' &&
      (envVars.consoleType === 'pro' || envVars.consoleType === 'pro-lite') &&
      envVars.ssoIdentityProviders?.length
    ) {
      loginMethods.push(
        ...envVars.ssoIdentityProviders.map((idp, i) => (
          <SSOLoginButton
            key={`idp-${idp.client_id}-${i}`}
            clientId={idp.client_id}
            name={idp.name}
            authorizationUrl={idp.authorization_url}
            scope={idp.scope}
          />
        )),
      );
    }

    if (envVars.consoleId) {
      loginMethods.push(<LoginWithHasura />);
    }

    if (!envVars.isAdminSecretDisabled) {
      loginMethods.push(
        <Button
          mode="default"
          size="3"
          onClick={doAdminSecretLogin}
          leftIcon={FaKey}
        >
          Sign In with Admin Secret
        </Button>,
      );
    }
    return loginMethods;
  };

  return showAdminSecretLogin ? (
    <AdminSecretLogin backToLoginHome={backToLoginHome} />
  ) : (
    <Flex direction="column" gap="2">
      {renderLoginMethods()}
    </Flex>
  );
};

export default LoginEE;
