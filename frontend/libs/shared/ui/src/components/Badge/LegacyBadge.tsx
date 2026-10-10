import React from 'react';
import clsx from 'clsx';

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
  className?: string;
  style?: React.CSSProperties;
}

const restApiBadgeClassName =
  'text-xs px-2 py-1 border border-solid border-[#AACBE0]';

const badgeConfig: Record<
  string,
  { label: string; bg: string; color: string; className?: string } | undefined
> = {
  error: { label: 'error', bg: '#FFE8E8', color: '#F47E7E' },
  community: { label: 'community', bg: '#D6EBFF', color: '#5C94C8' },
  'beta update': { label: 'beta update', bg: '#fff', color: '#4BB5AC' },
  update: { label: 'update', bg: '#FFEBCD', color: '#E49928' },
  feature: { label: 'feature', bg: '#FFEBCD', color: '#E49928' },
  'version update': { label: 'ver update', bg: '#fff', color: '#2EB67D' },
  security: { label: 'security', bg: '#FFE8E8', color: '#F47E7E' },
  experimental: { label: 'experimental', bg: '#DBEAFE', color: '#1E40AF' },
  'rest-GET': {
    label: 'GET',
    bg: '#e6f7ff',
    color: '#006699',
    className: restApiBadgeClassName,
  },
  'rest-PUT': {
    label: 'PUT',
    bg: '#e6f7ff',
    color: '#006699',
    className: restApiBadgeClassName,
  },
  'rest-POST': {
    label: 'POST',
    bg: '#e6f7ff',
    color: '#006699',
    className: restApiBadgeClassName,
  },
  'rest-PATCH': {
    label: 'PATCH',
    bg: '#e6f7ff',
    color: '#006699',
    className: restApiBadgeClassName,
  },
  'rest-DELETE': {
    label: 'DELETE',
    bg: '#e6f7ff',
    color: '#006699',
    className: restApiBadgeClassName,
  },
};

export const LegacyBadge: React.FC<BadgeProps> = ({
  type = '',
  className,
  style,
}) => {
  const config = badgeConfig[type];
  if (!config) {
    return null;
  }

  return (
    <span
      className={clsx(
        'uppercase tracking-[0.4px] font-bold text-[10px] leading-3 rounded-[84px] py-2 px-3',
        config.className,
        className,
      )}
      style={{ backgroundColor: config.bg, color: config.color, ...style }}
    >
      {config.label}
    </span>
  );
};
