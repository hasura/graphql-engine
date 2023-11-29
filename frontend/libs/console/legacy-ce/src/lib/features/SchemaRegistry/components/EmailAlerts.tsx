import React from 'react';
import { Checkbox } from '../../../new-components/Form';
import { FaClock, FaBan, FaBell } from 'react-icons/fa';
import { useGetAlertConfig } from '../hooks/useGetAlertConfig';
import { CapitalizeFirstLetter } from '../utils';
import globals from '../../../Globals';
import { AlertHeader } from './AlertsHeader';
import { useSetEmailAlertConfig } from '../hooks/useSetAlertConfig';
import { CustomDialogFooter } from './CustomDialogFooter';
import { ConfigKey } from '../types';

type EmailAlertsProps = {
  onClose: () => void;
};

const configKeys: ConfigKey[] = ['breaking', 'dangerous', 'safe'];

const defaultAlertConfig: Record<string, boolean> = {
  safe: false,
  breaking: false,
  dangerous: false,
};

export const EmailAlerts: React.FC<EmailAlertsProps> = ({ onClose }) => {
  const projectID = globals.hasuraCloudProjectId || '';
  const fetchAlertConfigResponse = useGetAlertConfig(projectID, 'mail');

  const [config, setConfig] =
    React.useState<Record<ConfigKey, boolean>>(defaultAlertConfig);

  const { kind } = fetchAlertConfigResponse;
  const alertConfig =
    kind === 'success' &&
    fetchAlertConfigResponse.response.alert_config_service.length
      ? fetchAlertConfigResponse.response.alert_config_service[0].rules
      : defaultAlertConfig;

  const { setEmailAlertMutation } = useSetEmailAlertConfig(onClose);

  const onSet = React.useCallback(() => {
    setEmailAlertMutation.mutate({ projectId: projectID, rules: config });
  }, [config]);

  // initialise checkboxes
  React.useEffect(() => {
    if (kind === 'success') {
      setConfig(alertConfig);
    }
  }, [alertConfig]);

  return (
    <div className="ml-[-14px]">
      {kind === 'loading' ? (
        <AlertHeader
          icon={<FaClock className="w-9 h-9 mt-sm mr-md fill-current" />}
          title="Loading..."
        />
      ) : kind === 'error' ? (
        <AlertHeader
          icon={<FaBan className="w-9 h-9 mt-sm mr-md fill-red-500" />}
          title="Error"
          description={fetchAlertConfigResponse.message}
        />
      ) : (
        <>
          <AlertHeader
            icon={<FaBell className="w-9 h-9 mt-sm mr-md fill-current" />}
            title="Email Alerts"
            description="Select the change categories for which an email should be
            sent!"
          />

          <div className="flex flex-col ml-8">
            {configKeys.map(c => {
              return (
                <div
                  className="flex items-center mb-xs cursor-pointer w-auto"
                  role="checkbox"
                  onClick={() => {
                    setConfig(prevConfig => ({
                      ...prevConfig,
                      [c]: !prevConfig[c],
                    }));
                  }}
                >
                  <Checkbox checked={config[c]} />
                  <p>{CapitalizeFirstLetter(c)}&nbsp;changes</p>
                </div>
              );
            })}
          </div>
        </>
      )}
      <CustomDialogFooter
        onSet={onSet}
        onClose={onClose}
        isLoading={setEmailAlertMutation.isLoading}
      />
    </div>
  );
};
