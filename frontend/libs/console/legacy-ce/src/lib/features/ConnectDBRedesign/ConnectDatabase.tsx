import React from 'react';
import { EELiteAccess } from '../EETrial';
import { ConnectDbBody } from './ConnectDbBody';
import { ConnectDatabaseWrapper, FancyRadioCards } from './components';
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

  // const [selectedDriver, setSelectedDriver] =
  //   React.useState<DriverInfo>(DEFAULT_DRIVER);
  const [selectedDriverName, setSelectedDriverName] = React.useState(
    DEFAULT_DRIVER.name
  );

  const { cardData, allDrivers, availableDrivers } = useDatabaseConnectDrivers({
    showEnterpriseDrivers: consoleType !== 'oss',
    onFirstSuccess: () =>
      setSelectedDriverName(
        current =>
          allDrivers.find(
            d =>
              d.name === initialDriverName &&
              (d.enterprise === false || consoleType !== 'oss')
          )?.name || current
      ),
  });

  // this needs to be a reactive value hence the useMemo usage.
  // when "allDrivers" changes due to a react query invalidation/metadata reload, the properties of the driver may change
  // in order for this to reflect automatically, we make this value dependant on both the state of "allDrivers" array and the "selectedDriverName" string
  const selectedDriver = React.useMemo(
    () => allDrivers.find(d => d.name === selectedDriverName) || DEFAULT_DRIVER,
    [allDrivers, selectedDriverName]
  );

  const isDriverAvailable = (availableDrivers ?? []).some(
    d => d.name === selectedDriver.name
  );

  return (
    <ConnectDatabaseWrapper>
      <FancyRadioCards
        items={cardData}
        value={selectedDriver?.name}
        onChange={val => {
          setSelectedDriverName(val);
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
