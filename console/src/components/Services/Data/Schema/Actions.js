import gqlPattern, { gqlSchemaErrorNotif } from '../Common/GraphQLValidation';
import { showErrorNotification } from '../../Common/Notification';
import { makeMigrationCall, fetchSchemaList } from '../DataActions';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { getRunSqlQuery } from '../../../Common/utils/v1QueryUtils';

const getDropSchemaSql = schemaName => `drop schema "${schemaName}" cascade;`;

const getCreateSchemaSql = schemaName => `create schema "${schemaName}";`;

export const createNewSchema = (schemaName, successCb, errorCb) => {
  return (dispatch, getState) => {
    if (!gqlPattern.test(schemaName)) {
      return dispatch(
        showErrorNotification(
          gqlSchemaErrorNotif[0],
          gqlSchemaErrorNotif[1],
          gqlSchemaErrorNotif[2]
        )
      );
    }

    const migrationUp = [getRunSqlQuery(getCreateSchemaSql(schemaName))];

    const migrationDown = [getRunSqlQuery(getDropSchemaSql(schemaName))];

    const migrationName = `create_schema_${schemaName}`;
    const requestMsg = 'Creating schema';
    const successMsg = 'Successfully created schema';
    const errorMsg = 'Error creating schema';

    const customOnSuccess = () => {
      dispatch(fetchSchemaList()).then(() => {
        if (successCb) {
          successCb();
        }
      });
    };
    const customOnError = () => {
      if (errorCb) {
        errorCb();
      }
    };

    makeMigrationCall(
      dispatch,
      getState,
      migrationUp,
      migrationDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const deleteCurrentSchema = (successCb, errorCb) => {
  return (dispatch, getState) => {
    const { currentSchema } = getState().tables;

    if (currentSchema === 'public') {
      return dispatch(
        showErrorNotification('Dropping "public" schema is not supported')
      );
    }

    const confirmMessage = `This will permanently delete the Postgres schema "${currentSchema}" from the database`;
    const isOk = getConfirmation(confirmMessage, true, currentSchema);
    if (!isOk) {
      return;
    }

    const migrationUp = [getRunSqlQuery(getDropSchemaSql(currentSchema))];
    const migrationName = `drop_schema_${currentSchema}`;
    const requestMsg = 'Dropping schema';
    const successMsg = 'Successfully dropped schema';
    const errorMsg = 'Error dropping schema';

    const customOnSuccess = () => {
      dispatch(fetchSchemaList());
      if (successCb) {
        successCb();
      }
    };
    const customOnError = () => {
      if (errorCb) {
        errorCb();
      }
    };

    makeMigrationCall(
      dispatch,
      getState,
      migrationUp,
      [],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};
