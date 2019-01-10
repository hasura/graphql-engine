import Notifications from 'react-notification-system-redux';
import { setNotificationShownInDB } from './Actions';

const message =
  'Help us improve Hasura! The console collects anonymized usage stats which allows us to keep improving Hasura at warp speed. Click read more to opt-out or read more';

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
        autoDismiss: 5,
        level: 'info',
        title: 'Telemetry',
        message: message,
        onRemove: () => dispatch(onRemove()),
      })
    );
  };
};

export { showTelemetryNotification };
