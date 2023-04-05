import React from 'react';
import { DriverInfo } from '../DataSource';
import { EELiteAccess } from '../EETrial';
import { ConnectDatabaseWrapper, FancyRadioCards } from './components';
import { ConnectDbBody } from './ConnectDbBody';
import { DEFAULT_DRIVER } from './constants';
import { useDatabaseConnectDrivers } from './hooks/useConnectDatabaseDrivers';
import { DbConnectConsoleType } from './types';

export type ConnectDatabaseProps = {
  /**
   *
   * Can be used to set initial selected database. Will default to Postgres if not set.
   *
   */
  initialDriverName?: string;

  /**
   *
   * Used to drive the rendering of body content after the radio cards
   *
   */
  consoleType: DbConnectConsoleType;
  /**
   *
   * Possible license statuses that are relevant to ProLite
   *
   */
  eeLicenseInfo: EELiteAccess['access'];
};

export const ConnectDatabaseV2 = (props: ConnectDatabaseProps) => {
  const { initialDriverName, eeLicenseInfo, consoleType } = props;

  const [selectedDriver, setSelectedDriver] =
    React.useState<DriverInfo>(DEFAULT_DRIVER);

  const { cardData, allDrivers, availableDrivers } = useDatabaseConnectDrivers({
    showEnterpriseDrivers: consoleType !== 'oss',
    onFirstSuccess: () =>
      setSelectedDriver(
        currentDriver =>
          allDrivers.find(
            d =>
              d.name === initialDriverName &&
              (d.enterprise === false || consoleType !== 'oss')
          ) || currentDriver
      ),
  });

  const isDriverAvailable = (availableDrivers ?? []).some(
    d => d.name === selectedDriver.name
  );

  return (
    <ConnectDatabaseWrapper>
      <FancyRadioCards
        items={cardData}
        value={selectedDriver?.name}
        onChange={val => {
          setSelectedDriver(
            prev => allDrivers?.find(d => d.name === val) || prev
          );
        }}
      />
      <ConnectDbBody
        consoleType={consoleType}
        selectedDriver={selectedDriver}
        eeLicenseInfo={eeLicenseInfo}
        isDriverAvailable={isDriverAvailable}
      />
    </ConnectDatabaseWrapper>
  );
};
