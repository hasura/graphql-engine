import React from 'react';
import { Dialog } from '../../../new-components/Dialog';
import { Checkbox } from '../../../new-components/Form';
import { FaClock, FaBan, FaBell } from 'react-icons/fa';
import { useGetAlertConfig } from '../hooks/useGetAlertConfig';
import { CapitalizeFirstLetter } from '../utils';
import globals from '../../../Globals';
import { AlertHeader } from './AlertsHeader';
import { CustomDialogFooter } from './CustomDialogFooter';
import { ConfigKey } from '../types';

type DialogProps = {
  onClose: () => void;
};

const configKeys: ConfigKey[] = ['breaking', 'dangerous', 'safe'];

const defaultAlertConfig: Record<string, boolean> = {
  safe: true,
  breaking: true,
  dangerous: true,
};

export const AlertsDialog: React.FC<DialogProps> = ({ onClose }) => {
  const projectID = globals.hasuraCloudProjectId || '';
  const fetchAlertConfigResponse = useGetAlertConfig(projectID);

  const [config, setConfig] =
    React.useState<Record<ConfigKey, boolean>>(defaultAlertConfig);

  const { kind } = fetchAlertConfigResponse;
  const alertConfig =
    kind === 'success' &&
    fetchAlertConfigResponse.response.schema_registry_alerts.length
      ? fetchAlertConfigResponse.response.schema_registry_alerts[0].config
      : defaultAlertConfig;

  // initialise checkboxes
  React.useEffect(() => {
    if (kind === 'success') {
      setConfig(alertConfig);
    }
  }, [alertConfig]);

  return (
    <Dialog
      hasBackdrop
      onClose={kind === 'error' ? onClose : undefined}
      footer={
        kind === 'success' ? (
          <CustomDialogFooter onClose={onClose} alertConfig={config} />
        ) : undefined
      }
    >
      {kind === 'loading' ? (
        <div className="flex items-top p-md">
          <AlertHeader
            icon={<FaClock className="w-9 h-9 mt-sm mr-md fill-current" />}
            title="Loading..."
          />
        </div>
      ) : kind === 'error' ? (
        <div className="flex items-top p-md">
          <AlertHeader
            icon={<FaBan className="w-9 h-9 mt-sm mr-md fill-red-500" />}
            title="Error"
            description={fetchAlertConfigResponse.message}
          />
        </div>
      ) : (
        <>
          <div className="flex items-top p-md">
            <div className="text-yellow-500">
              <FaBell className="w-9 h-9 mt-sm mr-md fill-current" />
            </div>
            <div>
              <p className="font-semibold">Email Alerts</p>
              <div className="overflow-y-auto max-h-[calc(100vh-14rem)]">
                <p className="m-0">
                  Select the change categories for which an email should be
                  sent!
                </p>
              </div>
            </div>
          </div>
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
    </Dialog>
  );
};
