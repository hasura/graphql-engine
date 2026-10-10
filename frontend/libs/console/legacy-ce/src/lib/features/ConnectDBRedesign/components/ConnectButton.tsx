import { useQueryClient } from '@tanstack/react-query';
import { Button, useHasuraAlert, hasuraToast, Text } from '@hasura/shared/ui';
import { useReloadMetadata } from '@hasura/metadata/api';
import { DriverInfo } from '@hasura/metadata/data-source';
import { dataRoutes, getErrorMessage } from '@hasura/shared/utils';
import { useNavigate } from 'react-router';
import { Code } from '@radix-ui/themes';

export const ConnectButton = ({
  selectedDriver,
  isDriverAvailable,
}: {
  selectedDriver: DriverInfo;
  isDriverAvailable?: boolean;
}) => {
  const pushRoute = useNavigate();
  const { hasuraConfirm } = useHasuraAlert();
  const { reloadMetadata } = useReloadMetadata();
  const client = useQueryClient();

  const connectionIssue = !isDriverAvailable;

  const handleClick = () => {
    if (connectionIssue) {
      hasuraConfirm({
        message: (
          <div className="pb-4">
            <Text as="p">
              The selected driver <Code>{selectedDriver.displayName}</Code>{' '}
              cannot be reached at the moment.
            </Text>
            <Text as="p">
              This is usually due to a connection issue and can be resolved by
              reloading metadata.
            </Text>
            <Text as="p">If this issue persists, please contact support.</Text>
          </div>
        ),
        title: 'Driver Error',
        confirmText: 'Reload Metadata',

        onCloseAsync: async ({ confirmed }) => {
          if (!confirmed)
            return {
              withSuccess: false,
            };

          try {
            await reloadMetadata({
              shouldReloadAllSources: false,
              shouldReloadRemoteSchemas: false,
            });

            client.invalidateQueries();
            return { withSuccess: true, successText: 'Metadata Reloaded' };
          } catch (err) {
            hasuraToast({
              message: getErrorMessage(err),
              title: 'There was an error reloading your metadata.',
              type: 'error',
            });

            return {
              withSuccess: false,
            };
          }
        },
      });
    } else {
      pushRoute(dataRoutes.connectDatabase(selectedDriver.name));
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
