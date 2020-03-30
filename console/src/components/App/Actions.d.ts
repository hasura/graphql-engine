import React from 'react';

declare const LOAD_REQUEST: string;
declare const DONE_REQUEST: string;
declare const FAILED_REQUEST: string;
declare const ERROR_REQUEST: string;

interface NotificationOpts extends React.ComponentProps<'div'> {
  level?: string;
  title: string;
  message?: string | null;
  position?: string;
  action?: {
    label: string;
    callback: () => void;
  } | null;
  autoDismiss?: number;
}

declare function showNotification(opts: NotificationOpts): void;

export {
  LOAD_REQUEST,
  DONE_REQUEST,
  FAILED_REQUEST,
  ERROR_REQUEST,
  showNotification,
};
