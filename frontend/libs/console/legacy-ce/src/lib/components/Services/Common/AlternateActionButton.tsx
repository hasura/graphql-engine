import { useGetAnalyticsAttributes } from '../../../features/Analytics';
import React, { ReactText } from 'react';

type Props = {
  label: ReactText;
  onClick: () => void;
  trackId?: string;
};

/**
 * This button overrides the default action button of `react-notifications-system-redux`.
 * This could be used in places where we want to add some custom attributes to the action button,
 * like passing a tracking id.
 */
export const AlternateActionButton = (props: Props) => {
  const { label, onClick, trackId } = props;

  const analyticsAttributes = useGetAnalyticsAttributes(trackId);

  return (
    <div
      style={{
        margin: '0px',
        padding: '0px',
      }}
      {...analyticsAttributes}
    >
      {/* using the native button with copied styles from `react-notifications-system-redux` action button
      to keep it consistent with rest of the notifications system. */}
      <button
        style={{
          background: 'rgb(94, 164, 0)',
          borderRadius: '2px',
          padding: '6px 20px',
          fontWeight: 'bold',
          margin: '10px 0px 0px',
          border: '0px',
          color: 'rgb(255, 255, 255)',
        }}
        onClick={onClick}
      >
        {label}
      </button>
    </div>
  );
};
