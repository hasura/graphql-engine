/* eslint-disable import/no-mutable-exports */
import { useState, useEffect } from 'react';
import { Driver, DataSourcesAPI } from '.';
import { services } from './services';

export let currentDriver: Driver = 'postgres';
export let dataSource: DataSourcesAPI = services[currentDriver];

class DataSourceChangedEvent extends Event {
  static type = 'data-source-changed';
  constructor(public driver: Driver) {
    super(DataSourceChangedEvent.type);
  }
}
const eventTarget = new EventTarget();

export const setDriver = (driver: Driver) => {
  currentDriver = driver;
  dataSource = services[driver];

  eventTarget.dispatchEvent(new DataSourceChangedEvent(driver));
};

export const useDataSource = (): {
  driver: Driver;
  setDriver: (driver: Driver) => void;
  dataSource: DataSourcesAPI;
} => {
  const [driver, setState] = useState(currentDriver);

  useEffect(() => {
    const handleDriverChange = (event: Event) => {
      if (event instanceof DataSourceChangedEvent) {
        setState(event.driver);
      }
    };

    eventTarget.addEventListener(
      DataSourceChangedEvent.type,
      handleDriverChange
    );

    return () => {
      eventTarget.removeEventListener(
        DataSourceChangedEvent.type,
        handleDriverChange
      );
    };
  });

  return {
    driver,
    dataSource,
    setDriver,
  };
};

if ((module as any).hot) {
  // todo
  // (module as any).hot.dispose((data: any) => {
  //   data.driver = currentDriver;
  // });
  // (module as any).hot.accept(['./postgres', './mysql'], () => {
  //   currentDriver = (module as any).hot.data.driver;
  // });
}
