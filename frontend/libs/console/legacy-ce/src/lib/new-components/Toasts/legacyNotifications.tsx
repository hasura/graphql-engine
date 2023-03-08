import React, { MouseEvent } from 'react';
import AceEditor from 'react-ace';
import { toast, ToastOptions } from 'react-hot-toast/headless';
import { showModal } from '../../store/modal/modal.actions';
import { TableTrackingCustomizationModalKey } from '../../store/modal/modal.constants';
import { hasuraToast, ToastProps } from './hasuraToast';
import { Json } from '../../components/Common/utils/tsUtils';

export const showNotificationLegacy = hasuraToast;

export const showSuccessNotificationLegacy = (
  title: string,
  message?: string,
  noDismissNotifications?: boolean
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

type Error =
  | undefined
  | null
  | string
  | {
      message?:
        | {
            code?: number | string;
            error?:
              | {
                  message?: string;
                  code?: number | string;
                }
              | string;
            internal?: {
              error: {
                message?: string;
              };
            };
          }
        | string;
      info?: string;
      code?: number | string;
      error?:
        | {
            message: string;
          }
        | string;
      custom?: string;
      internal?: {
        error?: {
          message?: string;
          description: string;
        };
      };
      path?: string;
      callToAction?: string;
    };

export const getErrorMessage = (
  message: string | null,
  error?: Error
): string => {
  if (!error) {
    return message ?? '';
  }

  if (typeof error === 'string') {
    return error;
  }

  if (
    typeof error?.message !== 'string' &&
    (error?.message?.error === 'postgres query error' ||
      error?.message?.error === 'query execution failed')
  ) {
    if (error.message?.internal) {
      return `${error.message.code}: ${error.message.internal.error.message}`;
    }

    return `${error.code}: ${error.message.error}`;
  }

  if (error?.info) {
    return error.info ?? '';
  }

  if (error?.message) {
    if (error.code) {
      if (
        typeof error?.message !== 'string' &&
        error?.message?.error &&
        typeof error.message.error !== 'string'
      ) {
        return error.message.error?.message ?? '';
      }

      if (typeof error.message === 'string') {
        return error.message;
      }
    }

    if (typeof error.message === 'string') {
      return error.message;
    }

    if (typeof error?.message !== 'string' && error?.message?.code) {
      return `${error.message.code} : ${message}`;
    }

    if (typeof error.code === 'string') {
      return error.code ?? '';
    }

    if (typeof error.code === 'number') {
      return String(error.code);
    }
  }

  if (error?.internal && error?.internal && error?.internal?.error) {
    return `${error.internal?.error?.message}.${error.internal?.error?.description}`;
  }

  if (error?.custom) {
    return error.custom ?? '';
  }

  if (error?.code && error?.error && error?.path) {
    // Data API error
    if (typeof error.error === 'object') return error.error.message ?? '';

    return error.error ?? '';
  }

  if (error?.callToAction && message) {
    return message;
  }

  return '';
};

const getRefreshButton = (
  error: Record<string, any>
): Pick<ToastProps, 'button'> => {
  if (error && 'action' in error) {
    return {
      button: {
        label: 'Refresh Console',
        onClick: (event: MouseEvent) => {
          event.preventDefault();
          window.location.reload();
        },
      },
    };
  }
  return {};
};

const getErrorJson = (error: Record<string, any>): string | undefined => {
  let errorJson: string | undefined;

  if (error && typeof error === 'object') {
    if ('action' in error) {
      errorJson = error.action;
    } else if ('internal' in error) {
      errorJson = error.internal;
    } else if ('message' in error && typeof error.message !== 'string') {
      errorJson = error.message;
    }
  }

  return errorJson;
};

export const getNotificationDetails = (detailsJson: Json) => {
  return (
    <div className="my-2">
      <AceEditor
        readOnly
        showPrintMargin={false}
        mode="json"
        showGutter={false}
        theme="github"
        value={JSON.stringify(detailsJson, null, 4)}
        minLines={1}
        maxLines={25}
        setOptions={{ useWorker: false }}
      />
    </div>
  );
};

const getNotificationAction = (
  notificationToast: { id: string | undefined },
  title: string,
  error: Record<string, any>,
  errorJson: string | undefined,
  errorMessage: string
): Pick<ToastProps, 'button'> => {
  if (errorJson) {
    const children = [getNotificationDetails(errorJson)];
    return {
      button: {
        label: 'Details',
        onClick: () => {
          toast.dismiss(notificationToast.id);
          hasuraToast({
            type: 'error',
            title,
            message: errorMessage,
            children,
          });
        },
      },
    };
  }

  const isAddTableView = window.location.pathname.includes('table/add');
  /*
   * NOTE: this way of identifying specific errors from the server is very error-prone
   * https://hasurahq.atlassian.net/browse/NDAT-374 aims at finding a better solution
   */
  const conflictMessages = [
    'found duplicate fields',
    'Encountered conflicting definitions',
  ];
  const isConflictError = conflictMessages.some(conflictMessage =>
    errorMessage?.includes(conflictMessage)
  );
  if (isConflictError && !isAddTableView) {
    const action: Pick<ToastProps, 'button'> = {
      button: {
        label: 'Resolve Conflict',
        onClick: () => {
          showModal(TableTrackingCustomizationModalKey);
        },
      },
    };
    return action;
  }

  if (error?.callToAction === 'reload-metadata') {
    const action: Pick<ToastProps, 'button'> = {
      button: {
        label: 'Reload database metadata',
        onClick: () => {
          error?.callback();
        },
      },
    };
    return action;
  }

  return {};
};

export const showErrorNotificationLegacy = (
  title: string,
  message?: string | null,
  error?: Record<string, any>
) => {
  const notificationToast: { id: string | undefined } = { id: undefined };
  let toastProps: ToastProps = {
    type: 'error',
    title,
  };

  if (message) {
    toastProps.message = message;
  }

  if (error) {
    const errorMessage = getErrorMessage(message || '', error);
    const errorJson = getErrorJson(error);
    toastProps = {
      ...toastProps,
      message: errorMessage,
      ...getRefreshButton(error),
      ...getNotificationAction(
        notificationToast,
        title,
        error,
        errorJson,
        errorMessage
      ),
    };
  }

  notificationToast.id = hasuraToast(toastProps);
};

export const showWarningNotificationLegacy = (
  title: string,
  message: string,
  dataObj?: Json,
  child?: JSX.Element
) => {
  const toastProps: ToastProps = {
    type: 'warning',
    title,
  };

  if (message) {
    toastProps.message = message;
  }

  const children: JSX.Element[] = [];
  if (dataObj) {
    children.push(getNotificationDetails(dataObj));
  }
  if (child) {
    children.push(child);
  }

  hasuraToast({ ...toastProps, children });
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
