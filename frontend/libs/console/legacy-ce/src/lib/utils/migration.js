import { showErrorNotification } from '../components/Services/Common/Notification';
import globals from '../Globals';
import { SERVER_CONSOLE_MODE } from '../constants';

export const handleMigrationErrors = (title, errorMsg) => dispatch => {
  if (globals.consoleMode === SERVER_CONSOLE_MODE) {
    // handle errors for run_sql based workflow
    dispatch(showErrorNotification(title, errorMsg.code, errorMsg));
  } else if (errorMsg.code === 'migration_failed') {
    dispatch(showErrorNotification(title, 'Migration Failed', errorMsg));
  } else if (errorMsg.code === 'data_api_error') {
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(
      showErrorNotification(title, parsedErrorMsg.message.error, parsedErrorMsg)
    );
  } else {
    // any other unhandled codes
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(showErrorNotification(title, errorMsg.code, parsedErrorMsg));
  }
};
