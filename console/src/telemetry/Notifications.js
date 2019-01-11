import Notifications from 'react-notification-system-redux';
import { setNotificationShownInDB } from './Actions';

const message = `Help us improve Hasura!
The console collects anonymized usage stats which allows us to keep improving Hasura at warp speed.
<a href="https://docs.hasura.io/1.0/graphql/manual/guides/telemetry.html"
target="_blank">Click here</a> to read more or to opt-out.`;

const onRemove = () => {
  return dispatch => {
    dispatch(setNotificationShownInDB());
  };
};

const showTelemetryNotification = () => {
  return dispatch => {
    dispatch(
      Notifications.show({
        position: 'tr',
        autoDismiss: 10,
        level: 'info',
        title: 'Telemetry',
        message: message,
        onRemove: () => dispatch(onRemove()),
      })
    );
  };
};

export { showTelemetryNotification };
