import React, { ReactNode, ReactText } from 'react';
import AceEditor from 'react-ace';
import { toast } from 'react-hot-toast/headless';
import {
  hasuraToast,
  ToastProps,
  ToastType,
} from '../../../new-components/Toasts';
import { showModal } from '../../../store/modal/modal.actions';
import { TableTrackingCustomizationModalKey } from '../../../store/modal/modal.constants';
import { Button } from '../../../new-components/Button';
import { Thunk } from '../../../types';
import { Json } from '../../Common/utils/tsUtils';

import './Notification/NotificationOverrides.css';
import { isObject, isString } from '../../Common/utils/jsUtils';

import styles from './Notification/Notification.module.scss';
import { getAnalyticsAttributes } from '../../../features/Analytics';

export interface Notification {
  title?: string | JSX.Element;
  message?: string | JSX.Element;
  level?: 'error' | 'warning' | 'info' | 'success';
  position?: 'tr' | 'tl' | 'tc' | 'br' | 'bl' | 'bc';
  autoDismiss?: number;
  dismissible?: 'both' | 'button' | 'click' | 'hide' | 'none' | boolean;
  children?: React.ReactNode;
  uid?: number | string;
  action?: {
    label: string;
    callback?: () => void;
  };
  alternateActionButtonProps?: {
    label: ReactText;
    onClick: () => void;
    trackId?: string;
  };
  onRemove?: () => void;
}

/**
 * @deprecated Please use the new toast API /new-components/Toasts/hasuraToast.tsx
 */
export const showNotification = (
  options: Notification,
  level: ToastType,
  noDismissNotifications?: boolean
): Thunk => {
  return () => {
    const toastProps: ToastProps = {
      type: level,
    };
    let titleAsNode: ReactNode | undefined;
    let messageAsNode: ReactNode | undefined;

    if (level === 'success') {
      toast.remove();
    }

    if (options?.action && options?.action?.callback) {
      toastProps.button = {
        label: options.action.label,
        onClick: options.action.callback,
      };
    }

    if (options.alternateActionButtonProps) {
      toastProps.button = {
        label: options.alternateActionButtonProps.label,
        onClick: options.alternateActionButtonProps.onClick,
      };
      if (options.alternateActionButtonProps.trackId) {
        toastProps.button.dataAttributes = getAnalyticsAttributes(
          options.alternateActionButtonProps.trackId
        );
      }
    }

    if (typeof options?.title === 'object') {
      titleAsNode = options.title;
    } else if (typeof options?.title === 'string') {
      toastProps.title = options.title;
    }

    if (typeof options?.message === 'object') {
      messageAsNode = options.message;
    } else if (typeof options?.message === 'string') {
      toastProps.message = options.message;
    }

    toastProps.children = (
      <>
        {titleAsNode}
        {messageAsNode}
        {options.children}
      </>
    );

    hasuraToast(toastProps);
  };
};

export const getNotificationDetails = (
  detailsJson: Json,
  children: React.ReactNode
) => {
  return children || detailsJson ? (
    <div className="notification-details">
      <AceEditor
        readOnly
        showPrintMargin={false}
        mode="json"
        showGutter={false}
        theme="github"
        name="notification-details"
        value={JSON.stringify(detailsJson, null, 4)}
        minLines={1}
        maxLines={25}
        width="100%"
        setOptions={{ useWorker: false }}
      />
      {children}
    </div>
  ) : null;
};

// NOTE: this type has been created by reverse-engineering the original getErrorMessage function
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

/**
 * @deprecated Please use the new toast API /new-components/Toasts/hasuraToast.tsx
 */
const showErrorNotification = (
  title: string,
  message?: string | null,
  error?: Record<string, any>
): Thunk => {
  const getRefreshBtn = () => {
    if (error && 'action' in error) {
      return (
        <Button
          mode="primary"
          className={styles.add_mar_top_small}
          onClick={e => {
            e.preventDefault();
            window.location.reload();
          }}
        >
          Refresh Console
        </Button>
      );
    }
    return null;
  };

  const getErrorJson = () => {
    let errorJson;

    if (error && isObject(error)) {
      if ('action' in error) {
        errorJson = error.action;
      } else if ('internal' in error) {
        errorJson = error.internal;
      } else if ('message' in error && !isString(error.message)) {
        errorJson = error.message;
      }
    }

    return errorJson;
  };

  const errorMessage = getErrorMessage(message || '', error);
  const errorJson = getErrorJson();

  return dispatch => {
    const getNotificationAction = () => {
      if (errorJson) {
        const errorDetails = [
          getNotificationDetails(errorJson as Json, getRefreshBtn()),
        ];

        const action = {
          label: 'Details',
          callback: () => {
            dispatch(
              showNotification(
                {
                  position: 'br',
                  title,
                  message: errorMessage,
                  children: errorDetails,
                },
                'error'
              )
            );
          },
        };
        return action;
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
        const action = {
          label: 'Resolve Conflict',
          callback: () => {
            dispatch(showModal(TableTrackingCustomizationModalKey));
          },
        };
        return action;
      }

      if (error?.callToAction === 'reload-metadata') {
        const action = {
          label: 'Reload database metadata',
          callback: () => {
            error?.callback();
          },
        };
        return action;
      }

      return undefined;
    };

    const action = getNotificationAction();
    const errorDetails = [
      getNotificationDetails(errorJson as Json, getRefreshBtn()),
    ];
    dispatch(
      showNotification(
        {
          title,
          message: errorMessage,
          action,
          children: errorDetails,
        },
        'error'
      )
    );
  };
};

/**
 * @deprecated Please use the new toast API /new-components/Toasts/hasuraToast.tsx
 */
const showSuccessNotification = (
  title: string,
  message?: string,
  noDismiss?: boolean
): Thunk => {
  return dispatch => {
    dispatch(
      showNotification(
        {
          level: 'success',
          title,
          message,
        },
        'success',
        noDismiss
      )
    );
  };
};

/**
 * @deprecated Please use the new toast API /new-components/Toasts/hasuraToast.tsx
 */
const showInfoNotification = (title: string): Thunk => {
  return dispatch => {
    dispatch(
      showNotification(
        {
          title,
          autoDismiss: 0,
        },
        'info'
      )
    );
  };
};

/**
 * @deprecated Please use the new toast API /new-components/Toasts/hasuraToast.tsx
 */
const showWarningNotification = (
  title: string,
  message: string,
  dataObj?: Json,
  child?: JSX.Element
): Thunk => {
  const children: JSX.Element[] = [];
  if (dataObj) {
    const notificationDetails = getNotificationDetails(dataObj, null);
    if (notificationDetails) {
      children.push(notificationDetails);
    }
  }
  if (child) {
    children.push(child);
  }

  return dispatch => {
    dispatch(
      showNotification(
        {
          level: 'warning',
          title,
          message,
          children,
        },
        'warning'
      )
    );
  };
};

export {
  showErrorNotification,
  showSuccessNotification,
  showInfoNotification,
  showWarningNotification,
};
