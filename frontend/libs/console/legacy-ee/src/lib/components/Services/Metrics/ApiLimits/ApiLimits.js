import React from 'react';
import { FaAngleRight } from 'react-icons/fa';

import { Link } from 'react-router';

export const ApiLimits = () => {
  return (
    <div className="infoWrapper">
      <p>
        API Limits has been moved to{' '}
        <Link to="/api/security/api_limits">
          API&nbsp;
          <FaAngleRight className="hover:underline" aria-hidden="true" />
          &nbsp;Security
        </Link>
      </p>
    </div>
  );
};
