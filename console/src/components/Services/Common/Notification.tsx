import React from 'react';
import AceEditor from 'react-ace';
import { showNotification } from '../../App/Actions';
import Button from '../../Common/Button/Button';

import './Notification/NotificationOverrides.css';
import { isObject, isString } from '../../Common/utils/jsUtils';

const styles = require('./Notification/Notification.scss');

const getNotificationDetails = (detailsJson: any, children = null) => {
  return (
    <div className={'notification-details'}>
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

const showErrorNotification = (title: string, message: string, error?: any) => {
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
          notificationMessage =
            error.message.code + ': ' + error.message.internal.error.message;
        } else {
          notificationMessage = error.code + ': ' + error.message.error;
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
          notificationMessage = error.message.code + ' : ' + message;
        } else {
          notificationMessage = error.code;
        }
      } else if ('internal' in error && 'error' in error.internal) {
        notificationMessage = error.code + ' : ' + error.internal.error.message;
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
    let refreshBtn: JSX.Element | null;
    if (error && 'action' in error) {
      refreshBtn = (
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

  return (dispatch: any) => {
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
              showNotification({
                level: 'error',
                position: 'br', // HACK: to avoid expansion of existing notifications
                title,
                message: errorMessage,
                children: errorDetails,
              })
            );
          },
        };
      }

      return action;
    };

    dispatch(
      showNotification({
        level: 'error',
        title,
        message: errorMessage,
        action: getNotificationAction(),
      })
    );
  };
};

const showSuccessNotification = (title: string, message?: string) => {
  return (dispatch: any) => {
    dispatch(
      showNotification({
        level: 'success',
        title,
        message: message ? message : null,
      })
    );
  };
};

const showInfoNotification = (title: string) => {
  return (dispatch: any) => {
    dispatch(
      showNotification({
        title,
        autoDismiss: 0,
      })
    );
  };
};

const showWarningNotification = (title: string, message: string, dataObj: any) => {
  const children: JSX.Element[] = [];
  if (dataObj) {
    children.push(getNotificationDetails(dataObj));
  }

  return (dispatch: any) => {
    dispatch(
      showNotification({
        level: 'warning',
        title,
        message,
        children,
      })
    );
  };
};

export {
  showErrorNotification,
  showSuccessNotification,
  showInfoNotification,
  showWarningNotification,
};
