import { FiRefreshCw } from 'react-icons/fi';
import { hasuraToast } from '@hasura/shared/ui';
import { HttpError } from '@hasura/shared/types';
import { ADMIN_SECRET_HEADER_KEY } from '@hasura/shared/types';

const doesConflictCodeExist = (error: unknown) =>
  error &&
  typeof error === 'object' &&
  'code' in error &&
  error.code === 'conflict';

export const toastMetadataOutOfDateError = (
  error: unknown,
  invalidateMetadata: () => void,
) => {
  if (
    !doesConflictCodeExist(error) &&
    !(error instanceof HttpError && doesConflictCodeExist(error.data))
  ) {
    return false;
  }

  hasuraToast({
    title: 'Metadata is Out-of-Date',
    type: 'error',
    children: (
      <p>
        The operation failed as the metadata on the server is newer than what is
        currently loaded on the console.The metadata has to be re- fetched to
        continue editing it.
        <br />
        <br />
        Do you want fetch the latest metadata ?
      </p>
    ),
    button: {
      label: (
        <>
          <FiRefreshCw aria-hidden="true" /> Fetch metadata
        </>
      ),
      onClick: () => {
        invalidateMetadata();
      },
    },
  });

  return true;
};

export const handleMigrationStatusError = (
  err: unknown,
  hasGlobalAdminSecret: boolean,
) => {
  if (
    err instanceof HttpError &&
    typeof err.data === 'object' &&
    err.data?.code &&
    err.data?.code === 'data_api_error'
  ) {
    if (hasGlobalAdminSecret) {
      alert(`Hasura CLI: ${err.data.message || err.message}`);
    } else {
      alert(
        `Looks like CLI is not configured with the ${ADMIN_SECRET_HEADER_KEY}. Please configure and try again`,
      );
    }

    return;
  }

  alert(
    'Hasura console is not able to reach your Hasura GraphQL engine instance. Please ensure that your ' +
      'instance is running and the endpoint is configured correctly.',
  );
};
