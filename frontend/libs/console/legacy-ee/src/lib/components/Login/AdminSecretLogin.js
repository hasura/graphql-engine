import React from 'react';
import { generatedAdminSecretLoginConnector } from '@hasura/console-legacy-ce';
import { connect } from 'react-redux';

const AdminLogin = generatedAdminSecretLoginConnector(connect);

export const AdminSecretLogin = ({ backToLoginHome, children }) => {
  return (
    <AdminLogin>
      {children}
      {backToLoginHome && (
        <button
          type="button"
          onClick={backToLoginHome}
          className="flex items-center mb-sm text-gray-400 hover:text-gray-500 focus:text-gray-500 cursor-pointer"
        >
          <div className="flex items-start justify-center">
            <svg
              stroke="currentColor"
              fill="currentColor"
              strokeWidth="0"
              viewBox="0 0 320 512"
              height="1em"
              width="1em"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path d="M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z" />
            </svg>
            <label className="text-sm cursor-pointer">
              Back to SSO Sign In
            </label>
          </div>
        </button>
      )}
    </AdminLogin>
  );
};
