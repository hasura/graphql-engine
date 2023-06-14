import React from 'react';
import globals from '../../../Globals';
import { Button } from '../../../new-components/Button';
import { useSetEmailAlertConfig } from '../hooks/useSetAlertConfig';
import { ConfigKey } from '../types';

type CustomDialogFooterProps = {
  onClose: () => void;
  alertConfig: Record<ConfigKey, boolean>;
};

export const CustomDialogFooter: React.FC<CustomDialogFooterProps> = ({
  onClose,
  alertConfig,
}) => {
  const projectID = globals.hasuraCloudProjectId || '';
  const { setEmailAlertMutation } = useSetEmailAlertConfig(onClose);

  const onSet = React.useCallback(() => {
    setEmailAlertMutation.mutate({ projectId: projectID, config: alertConfig });
  }, [alertConfig]);

  return (
    <div className="flex justify-between border-t border-gray-300 bg-white p-sm">
      <div>
        <p className="text-muted">
          Email alerts will be sent to the owner of this project.
        </p>
      </div>
      <div className="flex">
        <Button onClick={onClose}>Cancel</Button>
        <div className="ml-2">
          <Button
            mode="primary"
            onClick={e => {
              e.preventDefault();
              onSet();
            }}
            isLoading={setEmailAlertMutation.isLoading}
            disabled={setEmailAlertMutation.isLoading}
          >
            Set
          </Button>
        </div>
      </div>
    </div>
  );
};
