import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';

import { getConfirmation } from '../../../Common/utils/jsUtils';

const fetchMigrationStatus = setMigrationStatus => dispatch => {
  const url = Endpoints.hasuractlMigrateStatus;
  const options = {
    method: 'GET',
    credentials: globalCookiePolicy,
  };
  return dispatch(requestAction(url, options)).then(data => {
    setMigrationStatus(data);
  });
};

const deleteMigrations = (migrations, setMigrationStatus) => dispatch => {
  const url = Endpoints.hasuractlMigrateDelete;
  const body = {
    migrations: migrations,
  };
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(body),
  };
  return dispatch(requestAction(url, options)).then(() => {
    dispatch(fetchMigrationStatus(setMigrationStatus));
  });
};

const squashMigrations = (migration, setMigrationStatus) => dispatch => {
  const url = Endpoints.hasuractlMigrateSquash;
  const body = {
    from: migration,
  };
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify(body),
  };
  return dispatch(requestAction(url, options)).then(data => {
    console.log(data);
    const confirmMessage = 'Do you want to delete the old migration files?';
    const isOk = getConfirmation(confirmMessage, true);
    if (!isOk) {
      return dispatch(fetchMigrationStatus(setMigrationStatus));
    }
    return dispatch(
      deleteMigrations(data.squashed_migrations, setMigrationStatus)
    );
  });
};

export default fetchMigrationStatus;

export { squashMigrations };
