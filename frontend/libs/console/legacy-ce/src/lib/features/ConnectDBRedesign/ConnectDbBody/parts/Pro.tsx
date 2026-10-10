import { DriverInfo } from '@hasura/metadata/data-source';
import { SetupConnector } from '../../components';
import { ConnectButton } from '../../components/ConnectButton';
import { useNavigate } from 'react-router';
import { dataRoutes } from '@hasura/shared/utils';

export const Pro = ({
  selectedDriver,
  isDriverAvailable,
}: {
  selectedDriver: DriverInfo;
  isDriverAvailable: boolean;
}) => {
  const navigate = useNavigate();
  return isDriverAvailable ? (
    <ConnectButton selectedDriver={selectedDriver} />
  ) : (
    <div className="mt-3" data-testid="setup-connector">
      <SetupConnector
        selectedDriver={selectedDriver}
        onSetupSuccess={() => {
          navigate(dataRoutes.connectDatabase(selectedDriver?.name));
        }}
      />
    </div>
  );
};
