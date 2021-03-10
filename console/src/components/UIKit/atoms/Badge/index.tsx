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
  | 'error'
  | 'experimental'
  | 'rest-GET'
  | 'rest-PUT'
  | 'rest-POST'
  | 'rest-PATCH'
  | 'rest-DELETE'
  | string;

interface BadgeProps {
  type: AllowedBadges;
}

interface ExtendedBadgeProps extends BadgeProps, StyledOwnBadgeProps {}

// TODO: update the colors for the methods badge
export const Badge: React.FC<ExtendedBadgeProps> = ({
  type = '',
  ...props
}) => {
  const restApiProps = {
    ...props,
    fontSize: '12px',
    px: '8px',
    py: '4px',
    border: '1px solid #AACBE0',
  };
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
    case 'experimental':
      return (
        <StyledBadge {...props} bg="#DBEAFE" color="#1E40AF">
          experimental
        </StyledBadge>
      );
    case 'rest-GET':
      return (
        <StyledBadge {...restApiProps} bg="#e6f7ff" color="#006699">
          GET
        </StyledBadge>
      );
    case 'rest-PUT':
      return (
        <StyledBadge {...restApiProps} bg="#e6f7ff" color="#006699">
          PUT
        </StyledBadge>
      );
    case 'rest-POST':
      return (
        <StyledBadge {...restApiProps} bg="#e6f7ff" color="#006699">
          POST
        </StyledBadge>
      );
    case 'rest-PATCH':
      return (
        <StyledBadge {...restApiProps} bg="#e6f7ff" color="#006699">
          PATCH
        </StyledBadge>
      );
    case 'rest-DELETE':
      return (
        <StyledBadge {...restApiProps} bg="#e6f7ff" color="#006699">
          DELETE
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
