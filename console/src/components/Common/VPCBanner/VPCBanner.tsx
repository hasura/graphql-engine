import React from 'react';
import { FaTimes, FaNetworkWired } from 'react-icons/fa';

export default function VPCBanner({
  className,
  onClose,
}: {
  className?: string;
  onClose?: VoidFunction;
}): JSX.Element {
  return (
    <div
      className={`flex items-center rounded bg-gray-200 border border-gray-300 py-sm px-sm mb-md ${className}`}
    >
      <FaNetworkWired className="fill-current self-start h-md text-muted" />
      <div className="ml-xs">
        <strong>Want to connect to a private database?</strong>
        <p>Explore our Dedicated VPC and VPC PrivateLink offerings.</p>
      </div>
      <a
        href="https://hasura.io/docs/latest/graphql/cloud/dedicated-vpc.html"
        target="__blank"
        className="font-semibold ml-auto mr-md px-sm py-xs font-base text-muted border border-muted rounded hover:bg-gray-300"
      >
        Learn More
      </a>
      {onClose && (
        <FaTimes
          className="fill-current cursor-pointer text-muted hover:text-gray-800"
          onClick={onClose}
        />
      )}
    </div>
  );
}
