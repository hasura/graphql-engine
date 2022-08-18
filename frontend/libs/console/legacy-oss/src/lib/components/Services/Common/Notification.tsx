import React from 'react';
import AceEditor from 'react-ace';
import {
  removeAll as removeNotifications,
  show as displayNotification,
  NotificationLevel,
} from 'react-notification-system-redux';
import { showModal } from '@/store/modal/modal.actions';
import { TableTrackingCustomizationModalKey } from '@/store/modal/modal.constants';
import Button from '../../Common/Button/Button';
import { Thunk } from '../../../types';
import { Json } from '../../Common/utils/tsUtils';

import './Notification/NotificationOverrides.css';
import { isObject, isString } from '../../Common/utils/jsUtils';

import styles from './Notification/Notification.module.scss';

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
}

export const showNotification = (
  options: Notification,
  level: NotificationLevel,
  noDismissNotifications?: boolean
): Thunk => {
  return dispatch => {
    if (level === 'success' && !noDismissNotifications) {
      dispatch(removeNotifications());
    }

    dispatch(
      displayNotification(
        {
          position: options.position || 'tr',
          autoDismiss: ['error', 'warning'].includes(level) ? 0 : 5,
          dismissible: ['error', 'warning'].includes(level)
            ? ('button' as any) // bug in @types/react-notification-system-redux types
            : ('click' as any),
          ...options,
        },
        level
      )
    );
  };
};

export const getNotificationDetails = (
  detailsJson: Json,
  children: React.ReactNode
) => {
  return (
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
      />
      {children}
    </div>
  );
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

const showErrorNotification = (
  title: string,
  message?: string | null,
  error?: Record<string, any>
): Thunk => {
  const getRefreshBtn = () => {
    if (error && 'action' in error) {
      return (
        <Button
          className={styles.add_mar_top_small}
          color="yellow"
          size="sm"
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
          getNotificationDetails(errorJson, getRefreshBtn()),
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
      if (errorMessage?.includes('found duplicate fields') && !isAddTableView) {
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
    dispatch(
      showNotification(
        {
          title,
          message: errorMessage,
          action,
        },
        'error'
      )
    );
  };
};

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

const showWarningNotification = (
  title: string,
  message: string,
  dataObj?: Json,
  child?: JSX.Element
): Thunk => {
  const children: JSX.Element[] = [];
  if (dataObj) {
    children.push(getNotificationDetails(dataObj, null));
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
