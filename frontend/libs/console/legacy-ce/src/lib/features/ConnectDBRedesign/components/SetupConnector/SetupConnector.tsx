import React from 'react';
import { GrConnect } from 'react-icons/gr';
import { Button } from '../../../../new-components/Button';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { DriverInfo } from '../../../DataSource';
import { DockerConfigDialog } from './parts/DockerConfigDialog';

export const SetupConnector: React.VFC<{
  selectedDriver: DriverInfo;
  onSetupSuccess: () => void;
}> = ({ selectedDriver, onSetupSuccess }) => {
  const [showSetup, setShowSetup] = React.useState(false);
  return (
    <>
      <IndicatorCard
        contentFullWidth
        customIcon={GrConnect}
        className="mt-3"
        status="info"
        showIcon
      >
        <div className="flex flex-col" data-testid="setup-data-connector-card">
          <div className="flex items-center  ">
            <div className="flex flex-col w-3/4">
              <div className="text-[21px] text-lg mb-3">
                Data Connector Required
              </div>
              <div className="text-md text-gray-700">
                {`The Hasura Data Connector Service is required for ${selectedDriver.displayName} databases.`}
              </div>
            </div>
            <div className="flex w-1/4 ml-2 justify-end">
              <Button mode="primary" onClick={() => setShowSetup(true)}>
                Setup Data Connector
              </Button>
            </div>
          </div>
        </div>
      </IndicatorCard>
      {showSetup && (
        <DockerConfigDialog
          selectedDriver={selectedDriver}
          onCancel={() => setShowSetup(false)}
          onSetupSuccess={onSetupSuccess}
        />
      )}
    </>
  );
};
