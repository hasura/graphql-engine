import React from 'react';

import StyledBadge, { StyledOwnBadgeProps } from './Badge';

interface BadgeProps {
  type: string;
}

interface ExtendedBadgeProps extends BadgeProps, StyledOwnBadgeProps { }

export const Badge: React.FC<ExtendedBadgeProps> = ({
  type = '',
  ...props
}) => {
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

  if (type === "error") {
    return (
      <StyledBadge {...props} bg="#E52D2D" color="white">
        error
      </StyledBadge>
    );
  }

  if (type !== '') {
    return (
      <StyledBadge {...props} bg="#001934" color="white">
        {type}
      </StyledBadge>
    );
  }

  return <StyledBadge {...props} color="white" />;
};

Badge.defaultProps = {
  fontSize: '11px',
  borderRadius: '2px',
  px: '4.25px',
  pt: '3px',
  pb: '2px',
};
