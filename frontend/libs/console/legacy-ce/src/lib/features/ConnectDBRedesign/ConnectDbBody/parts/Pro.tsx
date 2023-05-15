import { DriverInfo } from '../../../DataSource';
import { SetupConnector } from '../../components';
import { ConnectButton } from '../../components/ConnectButton';
import { usePushRoute } from '../../hooks';

export const Pro = ({
  selectedDriver,
  isDriverAvailable,
}: {
  selectedDriver: DriverInfo;
  isDriverAvailable: boolean;
}) => {
  const pushRoute = usePushRoute();
  return isDriverAvailable ? (
    <ConnectButton selectedDriver={selectedDriver} />
  ) : (
    <div className="mt-3" data-testid="setup-connector">
      <SetupConnector
        selectedDriver={selectedDriver}
        onSetupSuccess={() => {
          pushRoute(
            `/data/v2/manage/database/add?driver=${selectedDriver?.name}`
          );
        }}
      />
    </div>
  );
};
