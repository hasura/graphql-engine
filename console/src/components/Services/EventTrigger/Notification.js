import React from 'react';
import AceEditor from 'react-ace';
import { showNotification, showTempNotification } from '../../App/Actions';
import { notifExpand, notifMsg } from '../../App/Actions';

const styles = require('./TableCommon/Table.scss');

const showErrorNotification = (title, message, reqBody, error) => {
  let modMessage;
  let refreshBtn;
  if (
    error &&
    error.message &&
    (error.message.error === 'postgres query error' ||
      error.message.error === 'query execution failed')
  ) {
    if (error.message.internal) {
      modMessage =
        error.message.code + ': ' + error.message.internal.error.message;
    } else {
      modMessage = error.code + ': ' + error.message.error;
    }
  } else if (error && 'info' in error) {
    modMessage = error.info;
  } else if (error && 'message' in error) {
    if (error.code) {
      if (error.message.error) {
        modMessage = error.message.error.message;
      } else {
        modMessage = error.message;
      }
    } else if (error && error.message && 'code' in error.message) {
      modMessage = error.message.code + ' : ' + message;
    } else {
      modMessage = error.code;
    }
  } else if (error && 'internal' in error) {
    modMessage = error.code + ' : ' + error.internal.error.message;
  } else if (error && 'custom' in error) {
    modMessage = error.custom;
  } else if (error && 'code' in error && 'error' in error && 'path' in error) {
    // Data API error
    modMessage = error.error;
  } else {
    modMessage = error ? error : message;
  }
  let finalJson = error ? error.message : '{}';
  if (error && 'action' in error) {
    refreshBtn = (
      <button
        className={styles.yellow_button + ' ' + styles.add_mar_top_small}
        onClick={e => {
          e.preventDefault();
          window.location.reload();
        }}
      >
        Refresh Console
      </button>
    );
    finalJson = error.action;
  } else if (error && 'internal' in error) {
    finalJson = error.internal;
  }
  return dispatch => {
    const expandClicked = finalMsg => {
      // trigger a modal with a bigger view
      dispatch(notifExpand(true));
      dispatch(notifMsg(JSON.stringify(finalMsg, null, 4)));
    };
    dispatch(
      showNotification({
        level: 'error',
        title,
        message: modMessage,
        action: reqBody
          ? {
            label: 'Details',
            callback: () => {
              dispatch(
                showNotification({
                  level: 'error',
                  title,
                  message: modMessage,
                  dismissible: 'button',
                  children: [
                    <div className={styles.aceBlock}>
                      <i
                        onClick={e => {
                          e.preventDefault();
                          expandClicked(finalJson);
                        }}
                        className={styles.aceBlockExpand + ' fa fa-expand'}
                      />
                      <AceEditor
                        readOnly
                        showPrintMargin={false}
                        mode="json"
                        showGutter={false}
                        theme="github"
                        name="notification-response"
                        value={JSON.stringify(finalJson, null, 4)}
                        minLines={1}
                        maxLines={15}
                        width="100%"
                      />
                      {refreshBtn}
                    </div>,
                  ],
                })
              );
            },
          }
          : null,
      })
    );
  };
};

const showSuccessNotification = (title, message) => {
  return dispatch => {
    dispatch(
      showNotification({
        level: 'success',
        title,
        message: message ? message : null,
      })
    );
  };
};

const showTempErrorNotification = (title, message) => {
  return dispatch => {
    dispatch(
      showTempNotification({
        level: 'error',
        title,
        message: message ? message : null,
        autoDismiss: 3,
      })
    );
  };
};

const showInfoNotification = title => {
  return dispatch => {
    dispatch(
      showNotification({
        title,
        autoDismiss: 0,
      })
    );
  };
};

export {
  showErrorNotification,
  showSuccessNotification,
  showInfoNotification,
  showTempErrorNotification,
};
