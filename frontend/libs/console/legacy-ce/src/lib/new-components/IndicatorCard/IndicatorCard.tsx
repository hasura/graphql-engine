import React from 'react';
import clsx from 'clsx';
import { FaCheck, FaFlask, FaQuestion, FaTimes } from 'react-icons/fa';

type indicatorCardStatus = 'info' | 'positive' | 'negative' | 'experimental';

export type IndicatorCardProps = {
  status?: indicatorCardStatus;
  headline?: string;
  children?: React.ReactNode;
  showIcon?: boolean;
  className?: string;
  contentFullWidth?: boolean;
  customIcon?: React.VFC<{ className: string }>;
  id?: string;
};

const twCardColors: Record<indicatorCardStatus, string> = {
  info: 'border-l-secondary',
  negative: 'border-l-red-600',
  positive: 'border-l-green-600',
  experimental: `border-l-purple-600`,
};

const IconPerStatus: Record<
  indicatorCardStatus,
  React.VFC<{ className: string }>
> = {
  info: FaQuestion,
  negative: FaTimes,
  positive: FaCheck,
  experimental: FaFlask,
};

const iconColorsPerStatus: Record<indicatorCardStatus, string> = {
  info: 'text-blue-800 bg-indigo-100',
  positive: 'text-green-800 bg-green-100',
  negative: 'text-red-800 bg-red-100',
  experimental: 'text-purple-800 bg-purple-100',
};

export const IndicatorCard = ({
  status = 'info',
  headline,
  showIcon,
  children,
  className,
  contentFullWidth = false,
  customIcon,
  id,
}: IndicatorCardProps) => {
  const Icon = customIcon ?? IconPerStatus[status];

  return (
    <div
      className={clsx(
        'flex items-center bg-white rounded p-md border border-gray-300 border-l-4 mb-sm',
        twCardColors[status],
        className
      )}
    >
      {showIcon ? (
        <div
          className={clsx(
            'flex items-center justify-center h-xl w-xl rounded-full mr-md flex-shrink-0',
            iconColorsPerStatus[status]
          )}
        >
          <Icon className="h-4 fill-current stroke-2" />
        </div>
      ) : null}
      <div className={clsx(contentFullWidth && 'w-full')}>
        {headline ? (
          <p className="font-semibold" data-testid={headline}>
            {headline}
          </p>
        ) : null}
        <div>{children}</div>
      </div>
    </div>
  );
};
