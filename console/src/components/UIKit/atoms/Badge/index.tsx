import React from 'react';

import StyledBadge, { StyledOwnBadgeProps } from './Badge';

export type AllowedBadges =
  | ''
  | 'version update'
  | 'community'
  | 'beta update'
  | 'update'
  | 'feature'
  | 'security'
  | 'error';

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
    case 'error':
      return (
        <StyledBadge {...props} bg="#FFE8E8" color="#F47E7E">
          error
        </StyledBadge>
      );
    case 'community':
      return (
        <StyledBadge {...props} bg="#D6EBFF" color="#5C94C8">
          community
        </StyledBadge>
      );
    case 'beta update':
      return (
        <StyledBadge {...props} bg="#fff" color="#4BB5AC">
          beta update
        </StyledBadge>
      );
    case 'update':
      return (
        <StyledBadge {...props} bg="#FFEBCD" color="#E49928">
          update
        </StyledBadge>
      );
    case 'feature':
      return (
        <StyledBadge {...props} bg="#FFEBCD" color="#E49928">
          feature
        </StyledBadge>
      );
    case 'version update':
      return (
        <StyledBadge {...props} bg="#fff" color="#2EB67D">
          ver update
        </StyledBadge>
      );
    case 'security':
      return (
        <StyledBadge {...props} bg="#FFE8E8" color="#F47E7E">
          security
        </StyledBadge>
      );
    default:
      return null;
  }
};

Badge.defaultProps = {
  fontWeight: 'bold',
  fontSize: '10px',
  lineHeight: '12px',
  borderRadius: '84px',
  py: '8px',
  px: '12px',
  border: 'none',
  letterSpacing: '',
};
