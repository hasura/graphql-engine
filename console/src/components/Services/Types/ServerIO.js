import { makeMigrationCall } from '../Data/DataActions';
import {
  reformCustomTypes,
  hydrateTypeRelationships,
} from '../../../shared/utils/hasuraCustomTypeUtils';
import { getConfirmation } from '../../Common/utils/jsUtils';
import { exportMetadata } from '../../../metadata/actions';
import { customTypesSelector } from '../../../metadata/selector';
import { generateSetCustomTypesQuery } from '../../../metadata/queryUtils';
import Migration from '../../../utils/migration/Migration';

export const setCustomGraphQLTypes = (types, successCb, errorCb) => (
  dispatch,
  getState
) => {
  const isOk = getConfirmation(
    'This could have an effect on the dependent actions.'
  );
  if (!isOk) {
    successCb();
    return;
  }

  const existingTypes = customTypesSelector(getState());

  const hydratedTypes = hydrateTypeRelationships(types, existingTypes);

  const migration = new Migration();
  migration.add(
    generateSetCustomTypesQuery(reformCustomTypes(hydratedTypes)),
    generateSetCustomTypesQuery(reformCustomTypes(existingTypes))
  );

  const migrationName = 'set_custom_types';
  const requestMsg = 'Setting custom types...';
  const successMsg = 'Setting custom types successful';
  const errorMsg = 'Setting custom types failed';

  const customOnSuccess = () => {
    dispatch(exportMetadata(successCb));
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
