import { ToastOptions } from 'react-hot-toast/headless';
import { hasuraToast, ToastProps } from './hasuraToast';
import { getErrorContent } from '@hasura/shared/utils';
import { DisplayToastErrorMessage } from '../error/DisplayToastErrorMessage';
import { CodeBlock } from '../codeblock/CodeBlock/CodeBlock';

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
  // stack traces are only useful while developing the console
  const stack =
    process.env.NODE_ENV === 'development' && error instanceof Error
      ? error.stack
      : undefined;

  return hasuraToast({
    ...props,
    type: 'error',
    children: error ? (
      <>
        <DisplayToastErrorMessage message={getErrorContent(error)} />
        {stack ? (
          <div className="overflow-hidden py-1.5">
            <CodeBlock text={stack} scrollable height="200px" size="1" />
          </div>
        ) : null}
      </>
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
