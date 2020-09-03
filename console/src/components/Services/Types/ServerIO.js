import { makeMigrationCall } from '../Data/DataActions';
import {
  reformCustomTypes,
  hydrateTypeRelationships,
} from '../../../shared/utils/hasuraCustomTypeUtils';
import { generateSetCustomTypesQuery } from '../../Common/utils/v1QueryUtils';
import { getConfirmation } from '../../Common/utils/jsUtils';
import { exportMetadata } from '../../../metadata/actions';
import { customTypesSelector } from '../../../metadata/selector';

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

  const upQuery = generateSetCustomTypesQuery(reformCustomTypes(hydratedTypes));
  const downQuery = generateSetCustomTypesQuery(
    reformCustomTypes(existingTypes)
  );

  const migrationName = 'set_custom_types';
  const requestMsg = 'Setting custom types...';
  const successMsg = 'Setting custom types successfull';
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
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};
