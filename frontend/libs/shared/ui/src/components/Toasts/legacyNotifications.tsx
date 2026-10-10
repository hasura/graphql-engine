import { ToastOptions } from 'react-hot-toast/headless';
import { hasuraToast, ToastProps } from './hasuraToast';
import { getErrorContent } from '@hasura/shared/utils';
import { DisplayToastErrorMessage } from '../error/DisplayToastErrorMessage';

export const showNotificationLegacy = hasuraToast;

export const showSuccessNotificationLegacy = (
  title: string,
  message?: string,
  noDismissNotifications?: boolean,
) => {
  const toastOptions: ToastOptions = noDismissNotifications
    ? { duration: 1000000 }
    : {};
  const toastProps: ToastProps = {
    type: 'success',
    title,
    toastOptions,
  };

  if (message) {
    toastProps.message = message;
  }

  hasuraToast(toastProps);
};

export type ShowErrorNotificationProps = Omit<
  ToastProps,
  'children' | 'type'
> & {
  error?: unknown;
};

export const showErrorNotification = ({
  error,
  ...props
}: ShowErrorNotificationProps) => {
  return hasuraToast({
    ...props,
    type: 'error',
    children: error ? (
      <DisplayToastErrorMessage message={getErrorContent(error)} />
    ) : undefined,
  });
};

export const showInfoNotificationLegacy = (title: string, message?: string) => {
  const toastProps: ToastProps = {
    type: 'info',
    title,
  };

  if (message) {
    toastProps.message = message;
  }

  hasuraToast(toastProps);
};
