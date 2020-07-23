import React from 'react';

import StyledBadge, { StyledOwnBadgeProps } from './Badge';

export type AllowedBadges =
  | 'version update'
  | 'community'
  | 'update'
  | 'event'
  | 'security';

interface BadgeProps {
  type: AllowedBadges;
}

interface ExtendedBadgeProps extends BadgeProps, StyledOwnBadgeProps {}

// NOTE: update the colors once they are decided
export const Badge: React.FC<ExtendedBadgeProps> = ({
  type = '',
  ...props
}) => {
  switch (type) {
    case 'event':
      return (
        <StyledBadge {...props} bg="#001934" color="white">
          event
        </StyledBadge>
      );
    case 'community':
      return (
        <StyledBadge {...props} bg="#F33C6C" color="white">
          community
        </StyledBadge>
      );
    case 'update':
      return (
        <StyledBadge {...props} bg="#55DED4" color="#001934">
          version update
        </StyledBadge>
      );
    case 'version update':
      return (
        <StyledBadge {...props} bg="#E52D2D" color="white">
          version update
        </StyledBadge>
      );
    case 'security':
      return (
        <StyledBadge {...props} bg="#001934" color="white">
          security
        </StyledBadge>
      );
    default:
      return null;
  }
};

Badge.defaultProps = {
  fontSize: '11px',
  borderRadius: '2px',
  px: '4.25px',
  pt: '3px',
  pb: '2px',
};
