import React from 'react';

import { StyledBadge } from './Badge';

export const Badge = props => {
  const { type } = props;

  if (type === 'event') {
    return (
      <StyledBadge {...props} bg="#001934" color="white">
        events
      </StyledBadge>
    );
  }

  if (type === 'community') {
    return (
      <StyledBadge {...props} bg="#F33C6C" color="white">
        community
      </StyledBadge>
    );
  }

  if (type === 'update') {
    return (
      <StyledBadge {...props} bg="#55DED4" color="#001934">
        version update
      </StyledBadge>
    );
  }

  return (
    type && (
      <StyledBadge {...props} bg="#001934" color="white">
        {type}
      </StyledBadge>
    )
  );
};

Badge.defaultProps = {
  fontSize: '11px',
  borderRadius: '2px',
  px: '4.25px',
  pt: '3px',
  pb: '2px',
};
