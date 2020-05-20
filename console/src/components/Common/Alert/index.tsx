import React from 'react';

export type AlertType = 'warning' | 'danger' | 'success';

interface AlertProps {
  type: AlertType;
  text: string;
}

/**
 * ## Alert Component
 *
 * @typedef Props
 * @param {AlertType} type
 * @param {string} text
 * @param {Props}
 */
const Alert: React.FC<AlertProps> = ({ type, text }) => (
  <div className={`hidden alert alert-${type}`} role="alert">
    {text}
  </div>
);

export default Alert;
