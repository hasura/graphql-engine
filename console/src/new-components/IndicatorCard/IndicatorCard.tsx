import React from 'react';
import clsx from 'clsx';
import { FaCheck, FaQuestion, FaTimes } from 'react-icons/fa';

type indicatorCardStatus = 'info' | 'positive' | 'negative';

export type IndicatorCardProps = {
  status?: indicatorCardStatus;
  headline?: string;
  children?: React.ReactNode;
  showIcon?: boolean;
};

const cardColors: Record<indicatorCardStatus, string> = {
  info: 'border-l-secondary',
  negative: 'border-l-red-600',
  positive: 'border-l-green-600',
};

const IconPerStatus: Record<
  indicatorCardStatus,
  React.VFC<{ className: string }>
> = {
  info: FaQuestion,
  negative: FaTimes,
  positive: FaCheck,
};

const iconColorsPerStatus: Record<indicatorCardStatus, string> = {
  info: 'text-blue-800 bg-indigo-100',
  positive: 'text-green-800 bg-green-100',
  negative: 'text-red-800 bg-red-100',
};

export const IndicatorCard = ({
  status = 'info',
  headline,
  showIcon,
  children,
}: IndicatorCardProps) => {
  const Icon = IconPerStatus[status];

  return (
    <div
      className={clsx(
        'flex items-center bg-white rounded p-md border border-gray-300 border-l-4 mb-sm',
        cardColors[status]
      )}
    >
      {showIcon ? (
        <div
          className={clsx(
            'flex items-center justify-center h-xl w-xl rounded-full mr-md',
            iconColorsPerStatus[status]
          )}
        >
          <Icon className="h-4 fill-current stroke-2" />
        </div>
      ) : null}
      <div>
        {headline ? <p className="font-semibold">{headline}</p> : null}
        <p>{children}</p>
      </div>
    </div>
  );
};
