import React from 'react';
import { AnyAction } from 'redux';
import { ThunkDispatch } from 'redux-thunk';
import { showNotification } from '../components/Services/Common/Notification';
import { setTelemetryNotificationShownInDB } from './Actions';
import { ReduxState } from '../types';

const onRemove = () => {
  return (dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>) => {
    dispatch(setTelemetryNotificationShownInDB());
  };
};

const showTelemetryNotification = () => {
  return (dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>) => {
    dispatch(
      showNotification(
        {
          position: 'tr',
          autoDismiss: 10,
          level: 'info',
          title: 'Telemetry',
          children: (
            <div>
              Help us improve Hasura! The console collects anonymized usage
              stats which allows us to keep improving Hasura at warp speed.
              <a
                href="https://hasura.io/docs/latest/graphql/core/guides/telemetry.html"
                target="_blank"
                rel="noopener noreferrer"
              >
                {' '}
                Click here
              </a>{' '}
              to read more or to opt-out.
            </div>
          ),
          onRemove: () => dispatch(onRemove()),
        },
        'info'
      )
    );
  };
};

export { showTelemetryNotification };
