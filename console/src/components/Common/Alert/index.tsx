import React from 'react';

export type AlertType = 'warning' | 'danger' | 'success';

interface AlertProps {
  type: AlertType;
  text: string;
}

const Alert: React.FC<AlertProps> = ({ type, text }) => (
  <div className={`hidden alert alert-${type}`} role="alert">
    {text}
  </div>
);

export default Alert;
