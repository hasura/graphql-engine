import { useQueryClient } from 'react-query';
import { useHasuraAlert } from '../../../new-components/Alert';
import { Button } from '../../../new-components/Button';
import { hasuraToast } from '../../../new-components/Toasts';
import { useReloadMetadata } from '../../hasura-metadata-api/useReloadMetadata';
import { usePushRoute } from '../hooks';
import { DriverInfo } from '../../DataSource';
import to from 'await-to-js';

export const ConnectButton = ({
  selectedDriver,
}: {
  selectedDriver: DriverInfo;
}) => {
  const pushRoute = usePushRoute();
  const { hasuraConfirm } = useHasuraAlert();
  const { reloadMetadata } = useReloadMetadata();
  const client = useQueryClient();

  const connectionIssue = !selectedDriver.available;

  const handleClick = () => {
    if (connectionIssue) {
      hasuraConfirm({
        message: (
          <>
            <p>The selected driver cannot be reached at the moment.</p>
            <p>
              This is usually due to a connection issue and can be resolved by
              reloading metadata.
            </p>
            <p>If this issue persists, please contact support.</p>
          </>
        ),
        title: 'Driver Error',
        confirmText: 'Reload Metadata',

        onCloseAsync: async ({ confirmed }) => {
          if (!confirmed) return;

          const [err, result] = await to(
            reloadMetadata({
              shouldReloadAllSources: false,
              shouldReloadRemoteSchemas: false,
            })
          );
          if (err) {
            hasuraToast({
              message: 'There was an error reloading your metadata.',
              title: 'Error',
              type: 'error',
            });
            return;
          }
          const { success } = result;

          if (success) {
            client.invalidateQueries();
            return { withSuccess: true, successText: 'Metadata Reloaded' };
          } else {
            hasuraToast({
              message: 'There was an error reloading your metadata.',
              title: 'Error',
              type: 'error',
            });
            return;
          }
        },
      });
    } else {
      pushRoute(`/data/v2/manage/database/add?driver=${selectedDriver.name}`);
    }
  };
  return (
    <Button
      className="mt-6 self-end"
      data-testid="connect-existing-button"
      onClick={handleClick}
    >
      Connect Existing Database
    </Button>
  );
};
