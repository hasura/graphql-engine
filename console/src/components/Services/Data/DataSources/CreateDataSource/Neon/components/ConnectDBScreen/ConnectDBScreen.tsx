import React from 'react';
import { Button } from '@/new-components/Button';
import neon_banner from './assets/neon_banner.png';

export function ConnectDBScreen() {
  return (
    <div className="border border-gray-300 shadow-md rounded bg-white p-md">
      <div className="flex items-center mb-xs">
        <span className="font-semibold flex items-center text-sm py-0.5 px-1.5 text-indigo-600 bg-indigo-100 rounded">
          New
        </span>
        <span className="ml-xs font-semibold flex items-center text-sm py-0.5 px-1.5 text-indigo-600 bg-indigo-100 rounded">
          Free
        </span>
      </div>
      <img src={neon_banner} alt="neon_banner" />
      <div className="mt-sm mb-sm text-gray-700 text-lg">
        <b>Hasura</b> + <b>Neon</b> are partners now!
      </div>
      <div className="flex justify-between items-center">
        <div className="text-md text-gray-700">
          The multi-cloud fully managed Postgres with a generous free tier. We
          separated storage and compute to offer autoscaling, branching, and
          bottomless storage.
        </div>
        <Button data-trackid="neon-connect-db-button" mode="primary" size="lg">
          Create Neon Database for free
        </Button>
      </div>
    </div>
  );
}
