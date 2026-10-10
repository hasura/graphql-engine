import { DriverInfo } from '@hasura/metadata/data-source';
import { ConnectButton } from '../../components/ConnectButton';

export const Oss = ({
  selectedDriver,
  isDriverAvailable,
}: {
  selectedDriver: DriverInfo;
  isDriverAvailable: boolean;
}) => (
  <ConnectButton
    selectedDriver={selectedDriver}
    isDriverAvailable={isDriverAvailable}
  />
);
