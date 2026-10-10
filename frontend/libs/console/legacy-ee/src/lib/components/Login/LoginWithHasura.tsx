import { clearConsoleAuthState } from '@hasura/console-legacy-ce';
import { initiateOAuthRequest } from '../../shared/auth/utils';
import { useLocation } from 'react-router';
import { Link, Button, HasuraIcon } from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';
import { ReactNode } from 'react';

const LoginWithHasura = ({
  shouldRedirectBack,
  children,
}: {
  shouldRedirectBack?: boolean;
  children?: ReactNode;
}) => {
  const location = useLocation();
  const { envVars } = useAppContext();

  const onClick = () => {
    clearConsoleAuthState();
    initiateOAuthRequest(envVars, location, shouldRedirectBack);
  };

  if (children) {
    return (
      <Link className="cursor-pointer!" color="gray" onClick={onClick}>
        {children}
      </Link>
    );
  }

  return (
    <div>
      <Button
        mode="default"
        type="button"
        size="3"
        leftIcon={HasuraIcon}
        onClick={onClick}
        full
      >
        Login with Hasura
      </Button>
    </div>
  );
};

export default LoginWithHasura;
