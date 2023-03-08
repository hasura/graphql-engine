import React from 'react';
import { Tooltip } from '@hasura/console-legacy-ce';
import { FaInfoCircle } from 'react-icons/fa';

const AllowListHeader = ({ title }) => {
  return (
    <React.Fragment>
      {title}
      <span>
        <Tooltip
          side="right"
          tooltipContentChildren="Allow lists are only enabled if you set HASURA_GRAPHQL_ENABLE_ALLOWLIST or
      --enable-allowlist"
        >
          <FaInfoCircle
            aria-hidden="true"
            style={{ fontSize: '18px', marginLeft: '10px' }}
          />
        </Tooltip>
      </span>
    </React.Fragment>
  );
};

export default AllowListHeader;
