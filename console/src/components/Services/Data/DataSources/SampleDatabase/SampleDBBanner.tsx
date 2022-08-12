import * as React from 'react';
import { Link } from 'react-router';

export const SampleDBBanner: React.FC<{ show: boolean }> = ({ show }) => {
  if (!show) {
    return null;
  }

  return (
    <div
      className="bg-slate-700 rounded text-white p-sm shadow-lg"
      id="onboarding-sample-db-banner"
    >
      <p className="font-semibold mb-xs text-white">
        You are current using a read-only sample database to test getting
        started with your GraphQL API
      </p>
      <p className="text-gray-300 mb-sm">
        Try creating your first queries using this data source and then connect
        a database to continue building your API.
      </p>
      <Link
        to="/data/manage/connect"
        href="#!"
        type="button"
        className="ml-auto inline-flex justify-center w-full space-x-1.5 items-center font-semibold bg-gradient-to-t border rounded shadow-sm focus:outline-none focus:bg-gradient-to-t focus:ring-2 focus:ring-offset-2 focus:ring-offset-slate-700 focus:ring-yellow-400 disabled:opacity-60 h-btn px-sm from-primary to-primary-light border-primary-dark hover:border-primary-darker focus:from-primary focus:to-primary disabled:border-primary-dark no-underline hover:no-underline"
      >
        <span className="flex items-center whitespace-nowrap text-gray-900">
          Connect Database
          <svg
            className="ml-1 w-3"
            stroke="currentColor"
            fill="currentColor"
            strokeWidth="0"
            viewBox="0 0 448 512"
            height="1em"
            width="1em"
            xmlns="http://www.w3.org/2000/svg"
          >
            <path d="M190.5 66.9l22.2-22.2c9.4-9.4 24.6-9.4 33.9 0L441 239c9.4 9.4 9.4 24.6 0 33.9L246.6 467.3c-9.4 9.4-24.6 9.4-33.9 0l-22.2-22.2c-9.5-9.5-9.3-25 .4-34.3L311.4 296H24c-13.3 0-24-10.7-24-24v-32c0-13.3 10.7-24 24-24h287.4L190.9 101.2c-9.8-9.3-10-24.8-.4-34.3z" />
          </svg>
        </span>
      </Link>
    </div>
  );
};
