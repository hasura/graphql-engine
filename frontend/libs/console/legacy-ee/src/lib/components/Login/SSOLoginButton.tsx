import { getOAuthAuthorizeUrl, initiateGeneralOAuthRequest } from './utils';
import { clearAdminSecretState } from '../AppState';

import ssoIcon from './black-building.svg';

type Props = {
  clientId: string;
  name: string;
  authorizationUrl: string;
  scope: string;
  location: Location;
  shouldRedirectBack: boolean;
};

const SSOLoginButton = ({
  clientId,
  name,
  authorizationUrl,
  scope,
  location,
  shouldRedirectBack,
}: Props) => {
  const onClick = () => {
    clearAdminSecretState();
    initiateGeneralOAuthRequest(
      getOAuthAuthorizeUrl(authorizationUrl, clientId, scope),
      location,
      shouldRedirectBack
    );
  };

  return (
    <div className="w-full max-w-md">
      <button
        type="button"
        onClick={onClick}
        className="w-full inline-flex space-x-1.5 items-center justify-center font-semibold bg-gradient-to-t border rounded shadow-sm focus:outline-none focus:bg-gradient-to-t focus:ring-2 focus:ring-offset-2 focus:ring-yellow-400 disabled:opacity-60 h-btn px-sm from-primary to-primary-light border-primary-dark hover:border-primary-darker focus:from-primary focus:to-primary disabled:border-primary-dark"
      >
        <img alt={name} className="flex w-4 mr-1.5" src={ssoIcon} />
        {name}
      </button>
    </div>
  );
};

export default SSOLoginButton;
