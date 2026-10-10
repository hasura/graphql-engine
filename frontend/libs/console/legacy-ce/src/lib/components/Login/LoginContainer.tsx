import React from 'react';
import { Flex } from '@radix-ui/themes';
import { Card, HasuraEELogo, HasuraLogo } from '@hasura/shared/ui';
import { CLI_CONSOLE_MODE } from '@hasura/shared/types';
import { isProConsole } from '@hasura/shared/utils';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import clsx from 'clsx';

type LoginProps = {
  children?: React.ReactNode;
};

const LoginContainer: React.FC<LoginProps> = ({ children }) => {
  useDocumentTitle('Login | Hasura');
  const { envVars } = useAppContext();

  const showLogo = isProConsole(envVars) ? (
    <HasuraEELogo className="w-36 h-auto" />
  ) : (
    <HasuraLogo className="w-36 h-auto" />
  );

  return (
    <Flex justify="center" align="center" className="min-h-screen w-full">
      <Flex id="login" justify="center" direction="column" gap="4">
        <Flex justify="center" className="mb-4">
          {showLogo}
        </Flex>
        <Card
          className={clsx(
            'shadow-md',
            (envVars.consoleMode === CLI_CONSOLE_MODE ||
              (envVars.isAdminSecretDisabled && !envVars.ssoEnabled)) &&
              !envVars.adminSecret
              ? 'w-[720px]'
              : 'w-[400px]',
          )}
          size="3"
        >
          {children}
        </Card>
      </Flex>
    </Flex>
  );
};

export default LoginContainer;
