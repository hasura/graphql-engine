import globals from '../../../../../../../../Globals';
import React from 'react';
import { NeonIcon } from './NeonIconSmall';
import { useShowNeonDashboardLink } from './utils';

type NeonDashboardLinkProps = {
  className?: string;
};

export function NeonDashboardLink() {
  const neonDashboardLink = `https://console.${globals.neonRootDomain}/app/projects`;

  return (
    <div className="flex items-center justify-start">
      <div className="flex items-center">
        <div className="mr-2">
          <NeonIcon />
        </div>
        <div className="text-sm text-gray-700">
          <a
            className="hover:no-underline"
            href={neonDashboardLink}
            target="_blank"
            rel="noopener noreferrer"
          >
            Neon Console
          </a>
          <span className="ml-1">- Manage your Neon databases</span>
        </div>
      </div>
    </div>
  );
}

export function NeonDashboardLinkWrapper(props: NeonDashboardLinkProps) {
  const { className } = props;
  const show = useShowNeonDashboardLink();

  if (!show) return null;

  return (
    <div className={className}>
      <NeonDashboardLink />
    </div>
  );
}
