import { DriverInfo } from '../../../DataSource';
import { ConnectButton } from '../../components/ConnectButton';

export const Oss = ({ selectedDriver }: { selectedDriver: DriverInfo }) => (
  <ConnectButton driverName={selectedDriver?.name} />
);
