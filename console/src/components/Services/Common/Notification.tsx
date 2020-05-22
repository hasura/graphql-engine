import React from 'react';
import AceEditor from 'react-ace';
import {
  removeAll as removeNotifications,
  show as displayNotification,
  NotificationLevel,
} from 'react-notification-system-redux';
import { Notification } from 'react-notification-system';
import Button from '../../Common/Button/Button';
import { Thunk } from '../../../types';
import { Json } from '../../Common/utils/tsUtils';

import './Notification/NotificationOverrides.css';
import { isObject, isString } from '../../Common/utils/jsUtils';

const styles = require('./Notification/Notification.scss');

const showNotification = (
  options: Notification,
  level: NotificationLevel
): Thunk => {
  return dispatch => {
    if (level === 'success') {
      dispatch(removeNotifications());
    }

    dispatch(
      displayNotification(
        {
          position: options.position || 'tr',
          autoDismiss: ['error', 'warning'].includes(level) ? 0 : 5,
          dismissible: ['error', 'warning'].includes(level),
          ...options,
        },
        level
      )
    );
  };
};

const getNotificationDetails = (
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

const showErrorNotification = (
  title: string,
  message: string,
  error?: any
): Thunk => {
  const getErrorMessage = () => {
    let notificationMessage;

    if (error) {
      if (isString(error)) {
        notificationMessage = error;
      } else if (
        error.message &&
        (error.message.error === 'postgres query error' ||
          error.message.error === 'query execution failed')
      ) {
        if (error.message.internal) {
          notificationMessage = `${error.message.code}: ${error.message.internal.error.message}`;
        } else {
          notificationMessage = `${error.code}: ${error.message.error}`;
        }
      } else if ('info' in error) {
        notificationMessage = error.info;
      } else if ('message' in error) {
        if (error.code) {
          if (error.message.error) {
            notificationMessage = error.message.error.message;
          } else {
            notificationMessage = error.message;
          }
        } else if (error.message && isString(error.message)) {
          notificationMessage = error.message;
        } else if (error.message && 'code' in error.message) {
          notificationMessage = `${error.message.code} : ${message}`;
        } else {
          notificationMessage = error.code;
        }
      } else if ('internal' in error && 'error' in error.internal) {
        notificationMessage = `${error.code} : ${error.internal.error.message}`;
      } else if ('custom' in error) {
        notificationMessage = error.custom;
      } else if ('code' in error && 'error' in error && 'path' in error) {
        // Data API error
        notificationMessage = error.error;
      }
    } else {
      notificationMessage = message;
    }

    return notificationMessage;
  };

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

  const errorMessage = getErrorMessage();
  const errorJson = getErrorJson();

  return dispatch => {
    const getNotificationAction = () => {
      let action;

      if (errorJson) {
        const errorDetails = [
          getNotificationDetails(errorJson, getRefreshBtn()),
        ];

        action = {
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
      }

      return action;
    };

    dispatch(
      showNotification(
        {
          title,
          message: errorMessage,
          action: getNotificationAction(),
        },
        'error'
      )
    );
  };
};

const showSuccessNotification = (title: string, message?: string): Thunk => {
  return dispatch => {
    dispatch(
      showNotification(
        {
          level: 'success',
          title,
          message,
        },
        'success'
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
  dataObj: Json
): Thunk => {
  const children: JSX.Element[] = [];
  if (dataObj) {
    children.push(getNotificationDetails(dataObj, null));
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
