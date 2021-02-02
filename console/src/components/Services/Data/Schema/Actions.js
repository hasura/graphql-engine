import gqlPattern, { gqlSchemaErrorNotif } from '../Common/GraphQLValidation';
import { showErrorNotification } from '../../Common/Notification';
import { makeMigrationCall, fetchSchemaList } from '../DataActions';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { dataSource } from '../../../../dataSources';
import { getRunSqlQuery } from '../../../Common/utils/v1QueryUtils';
import Migration from '../../../../utils/migration/Migration';

export const createNewSchema = (schemaName, successCb, errorCb) => {
  return (dispatch, getState) => {
    const source = getState().tables.currentDataSource;
    if (!gqlPattern.test(schemaName)) {
      return dispatch(
        showErrorNotification(
          gqlSchemaErrorNotif[0],
          gqlSchemaErrorNotif[1],
          gqlSchemaErrorNotif[2]
        )
      );
    }
    const migration = new Migration();
    migration.add(
      getRunSqlQuery(dataSource.getCreateSchemaSql(schemaName), source),
      getRunSqlQuery(dataSource.getDropSchemaSql(schemaName), source)
    );

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
      migration.upMigration,
      migration.downMigration,
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
    const { currentSchema, currentDataSource } = getState().tables;

    const confirmMessage = `This will permanently delete schema "${currentSchema}" from the database`;
    const isOk = getConfirmation(confirmMessage, true, currentSchema);
    if (!isOk) {
      return;
    }
    const migration = new Migration();
    migration.add(
      getRunSqlQuery(
        dataSource.getDropSchemaSql(currentSchema),
        currentDataSource
      )
    );
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
      migration.upMigration,
      migration.downMigration,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};
