import * as React from 'react';
import Spinner from '@/components/Common/Spinner/Spinner';
import { useNeonOAuth } from './useNeonOAuth';

// The UI for this component is a work in progress
// It only wires up the functionality of the hook
export const Neon = (props: { oauthString?: string }) => {
  const { startNeonOAuth, neonOauthStatus } = useNeonOAuth(props.oauthString);

  switch (neonOauthStatus.status) {
    case 'idle':
      return (
        <div className="w-1/2 justify-center items-center">
          <ConnectToNeonButton startNeonOAuth={startNeonOAuth} />
        </div>
      );
    case 'authenticating':
      return (
        <div className="w-1/2 justify-center items-center">
          <div>
            Authenticating with Neon DB... <Spinner />
          </div>
        </div>
      );

    case 'authenticated':
      return (
        <div className="w-1/2 justify-center items-center">
          <div>
            Authenticated. Hello {neonOauthStatus.email}! <Spinner />
          </div>
        </div>
      );

    case 'error':
      return (
        <div className="w-1/2 justify-center items-center">
          <ConnectToNeonButton startNeonOAuth={startNeonOAuth} />
          <p>Error authenticating: {neonOauthStatus.error.message}</p>
        </div>
      );
    default:
      // This is a TS protection that forces the developer to properly manage all the cases.
      // TS will complain when the developer adds new statuses without adding a corresponding
      // `case` here.
      return unreachable(neonOauthStatus);
  }
};

function ConnectToNeonButton(props: { startNeonOAuth: () => void }) {
  return (
    <div
      onClick={props.startNeonOAuth}
      className="flex cursor-pointer flex-col justify-center items-center bg-white w-1/2 h-[40px] border border-[#1FE699]"
      role="button"
    >
      <p>Connect to Neon</p>
    </div>
  );
}

// I disable the rule because
// @see: https://github.com/microsoft/TypeScript/issues/38881#issuecomment-637013353
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const unreachable = (status: never) => null;
