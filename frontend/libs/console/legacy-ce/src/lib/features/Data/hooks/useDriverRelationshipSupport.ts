import { useEffect, useState } from 'react';
import { isObject } from '../../../components/Common/utils/jsUtils';
import { Capabilities } from '@hasura/dc-api-types';
import { useDriverCapabilities } from './useDriverCapabilities';
import { useAvailableDrivers } from '../../ConnectDB';

type UseDriverRelationshipSupportArgs = {
  dataSourceName: string;
};

export const useDriverRelationshipSupport = ({
  dataSourceName,
}: UseDriverRelationshipSupportArgs) => {
  const capabiltiesResponse = useDriverCapabilities({ dataSourceName });
  const capabilities = capabiltiesResponse.data as Capabilities;

  const { data: availableDrivers } = useAvailableDrivers();

  const [driverSupportsLocalRelationship, setDriverSupportsLocalRelationship] =
    useState(false);

  const [
    driverSupportsRemoteRelationship,
    setDriverSupportsRemoteRelationship,
  ] = useState(false);

  useEffect(() => {
    const isCurrentDriverNative = availableDrivers?.find(
      driver => driver.name === dataSourceName
    )?.native;

    if (isCurrentDriverNative) {
      setDriverSupportsLocalRelationship(true);
      setDriverSupportsRemoteRelationship(true);
      return;
    }
    setDriverSupportsLocalRelationship(isObject(capabilities?.relationships));
    setDriverSupportsRemoteRelationship(
      isObject(capabilities?.queries?.foreach)
    );
  }, [dataSourceName, capabilities]);

  return {
    driverSupportsLocalRelationship,
    driverSupportsRemoteRelationship,
  };
};
