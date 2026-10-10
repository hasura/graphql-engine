import { clearConsoleAuthState } from '@hasura/console-legacy-ce';
import {
  getOAuthAuthorizeUrl,
  getOAuthRedirectUrl,
  initiateGeneralOAuthRequest,
} from '../../shared/auth/utils';
import { useLocation } from 'react-router';
import { Button } from '@hasura/shared/ui';
import { CgOrganisation } from 'react-icons/cg';
import { useAppContext } from '@hasura/shared/context';

type Props = {
  clientId: string;
  name: string;
  authorizationUrl: string;
  scope: string;
  shouldRedirectBack?: boolean;
};

const SSOLoginButton = ({
  clientId,
  name,
  authorizationUrl,
  scope,
  shouldRedirectBack = false,
}: Props) => {
  const { envVars } = useAppContext();
  const location = useLocation();

  const onClick = () => {
    clearConsoleAuthState();
    const redirectUri = getOAuthRedirectUrl(envVars.urlPrefix);

    initiateGeneralOAuthRequest(
      getOAuthAuthorizeUrl(authorizationUrl, clientId, scope, redirectUri),
      location,
      shouldRedirectBack,
    );
  };

  return (
    <div className="w-full">
      <Button
        full
        size="3"
        type="button"
        mode="default"
        onClick={onClick}
        leftIcon={CgOrganisation}
      >
        {name}
      </Button>
    </div>
  );
};

export default SSOLoginButton;
