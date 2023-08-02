import { useEffect, useState } from 'react';
import { isObject } from '../../../components/Common/utils/jsUtils';
import { Capabilities } from '@hasura/dc-api-types';
import { useDriverCapabilities } from './useDriverCapabilities';
import { useAvailableDrivers } from '../../ConnectDB';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';

type UseDriverRelationshipSupportArgs = {
  dataSourceName: string;
};

export const useDriverRelationshipSupport = ({
  dataSourceName,
}: UseDriverRelationshipSupportArgs) => {
  const capabilitiesResponse = useDriverCapabilities({ dataSourceName });
  const capabilities = capabilitiesResponse.data as Capabilities;

  const { data: metadataSource } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const { data: availableDrivers } = useAvailableDrivers();

  const [driverSupportsLocalRelationship, setDriverSupportsLocalRelationship] =
    useState(false);

  const [
    driverSupportsRemoteRelationship,
    setDriverSupportsRemoteRelationship,
  ] = useState(false);

  useEffect(() => {
    const isCurrentDriverNative = availableDrivers?.find(
      driver => driver.name === metadataSource?.kind
    )?.native;

    if (isCurrentDriverNative) {
      setDriverSupportsLocalRelationship(true);
      setDriverSupportsRemoteRelationship(true);
      return;
    }
    // local relationships are supported if the capabilities object includes the object "relationships" (not null or undefined)
    setDriverSupportsLocalRelationship(isObject(capabilities?.relationships));
    // remote relationships are supported if the capabilities object includes the object "foreach" into the "queries" object (not null or undefined)
    setDriverSupportsRemoteRelationship(
      isObject(capabilities?.queries?.foreach)
    );
  }, [dataSourceName, capabilities, availableDrivers, metadataSource]);

  return {
    driverSupportsLocalRelationship,
    driverSupportsRemoteRelationship,
  };
};
