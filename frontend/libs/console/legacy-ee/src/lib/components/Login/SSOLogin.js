import React from 'react';

export const SSOLogin = ({
  hasuraEELogo,
  ssoIcon,
  keyIcon,
  doSSOLogin,
  doAdminSecretLogin,
}) => {
  return (
    <div className="flex items-center justify-center w-full h-screen bg-gray-50 text-gray-900">
      <div className="w-full max-w-md">
        <div className="w-full mb-md">
          <img className="flex w-36 mx-auto" src={hasuraEELogo} />
        </div>
        <div className="w-full max-w-md space-y-md bg-white border border-gray-300 p-md rounded shadow mb-lg">
          <button
            type="button"
            onClick={doSSOLogin}
            className="w-full inline-flex space-x-1.5 items-center justify-center font-semibold bg-gradient-to-t border rounded shadow-sm focus:outline-none focus:bg-gradient-to-t focus:ring-2 focus:ring-offset-2 focus:ring-yellow-400 disabled:opacity-60 h-btn px-sm from-primary to-primary-light border-primary-dark hover:border-primary-darker focus:from-primary focus:to-primary disabled:border-primary-dark"
          >
            <img className="flex w-4 mr-1.5" src={ssoIcon} />
            Sign In with Single Sign-On
          </button>
          <div>
            <button
              type="button"
              onClick={doAdminSecretLogin}
              className="w-full inline-flex space-x-1.5 items-center justify-center font-semibold bg-gradient-to-t border rounded shadow-sm focus:outline-none focus:bg-gradient-to-t focus:ring-2 focus:ring-offset-2 focus:ring-yellow-400 disabled:opacity-60 h-btn px-sm bg-gray-50 from-transparent to-white border-gray-300 hover:border-gray-400 disabled:border-gray-300 focus:from-bg-gray-50 focus:to-bg-gray-50"
            >
              <img className="flex w-4 mr-1.5" src={keyIcon} />
              Sign In with Admin Secret
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};
