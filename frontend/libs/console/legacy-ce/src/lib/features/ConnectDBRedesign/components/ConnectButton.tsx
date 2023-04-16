import React from 'react';
import { Button } from '../../../new-components/Button';
import { usePushRoute } from '../hooks';

export const ConnectButton = ({ driverName }: { driverName: string }) => {
  const pushRoute = usePushRoute();

  return (
    <Button
      className="mt-6 self-end"
      data-testid="connect-existing-button"
      onClick={() =>
        pushRoute(`/data/v2/manage/database/add?driver=${driverName}`)
      }
    >
      Connect Existing Database
    </Button>
  );
};
