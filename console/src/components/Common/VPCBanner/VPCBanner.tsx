import React from 'react';

export default function VPCBanner({
  className,
}: {
  className?: string;
}): JSX.Element {
  return (
    <div
      className={`flex items-center rounded bg-white border border-gray-300 border-l-4 border-l-cloud py-sm px-sm mb-md ${className}`}
    >
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width="20"
        height="20"
        viewBox="0 0 24 24"
        type="network"
        aria-hidden="true"
        className="self-start mr-sm mt-1 text-cloud"
      >
        <g>
          <rect width="24" height="24" style={{ fill: 'none' }} />
        </g>
        <polygon
          points="22,11 22,3 15,3 15,6 9,6 9,3 2,3 2,11 9,11 9,8 11,8 11,18 15,18 15,21 22,21 22,13 15,13 15,16  13,16 13,8 15,8 15,11 "
          className="fill-current"
        />
      </svg>
      <div>
        <strong>Want to connect to a private database?</strong>
        <p>Explore our Dedicated VPC and VPC PrivateLink offerings.</p>
      </div>
      <a
        href="https://hasura.io/docs/latest/graphql/cloud/dedicated-vpc.html"
        target="__blank"
        className="font-semibold ml-auto px-sm py-xs font-base text-cloud border border-cloud rounded "
      >
        Learn More
      </a>
    </div>
  );
}
