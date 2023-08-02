import { DriverInfo } from '../../DataSource';
import { EELiteAccess } from '../../EETrial';
import { DbConnectConsoleType } from '../types';
import { Cloud, Oss, Pro, ProLite } from './parts';

type ConnectDbBodyProps = {
  consoleType: DbConnectConsoleType;
  selectedDriver: DriverInfo;
  isDriverAvailable: boolean;
  eeLicenseInfo: EELiteAccess['access'];
};
export const ConnectDbBody = ({
  consoleType,
  selectedDriver,
  isDriverAvailable,
  eeLicenseInfo,
}: ConnectDbBodyProps) => {
  switch (consoleType) {
    case 'oss':
      return <Oss selectedDriver={selectedDriver} />;
    case 'pro-lite':
      return (
        <ProLite
          selectedDriver={selectedDriver}
          eeLicenseInfo={eeLicenseInfo}
          isDriverAvailable={isDriverAvailable}
        />
      );
    case 'pro':
      return (
        <Pro
          selectedDriver={selectedDriver}
          isDriverAvailable={isDriverAvailable}
        />
      );
    case 'cloud':
      return (
        <Cloud
          selectedDriver={selectedDriver}
          isDriverAvailable={isDriverAvailable}
        />
      );
  }
};
