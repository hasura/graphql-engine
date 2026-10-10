import { useState } from 'react';
import {
  FeaturesCompatibility,
  getFeaturesCompatibility,
} from '@hasura/shared/utils';
import { loadLatestServerVersion, loadServerVersion } from '../../api';
import { LatestReleaseVersionResult } from '@hasura/shared/types';
import { Endpoints } from '@hasura/shared/context';
import { EnvVars } from '@hasura/shared/types';

export const useServerVersion = ({
  endpoints,
  envVars,
}: {
  envVars: EnvVars;
  endpoints: Endpoints;
}) => {
  const [serverVersion, setServerVersion] = useState(envVars.serverVersion);
  const [latestServerVersion, setLatestServerVersion] =
    useState<LatestReleaseVersionResult>({
      latest: '',
      prerelease: '',
    });
  const [featuresCompatibility, setFeaturesCompatibility] =
    useState<FeaturesCompatibility>({});

  const initServerVersion = async () => {
    const url = endpoints.version;
    const { version } = await loadServerVersion(url).catch((err) => {
      console.error('Failed to fetch server version:', err);
      return { version: serverVersion };
    });
    if (!version) {
      return '';
    }

    const featCompat = getFeaturesCompatibility(version);
    setServerVersion(version);
    setFeaturesCompatibility(featCompat);

    await loadLatestServerVersion(endpoints.updateCheck, version)
      .then((result) => {
        setLatestServerVersion(result);
      })
      .catch((err) => {
        console.error('Failed to fetch latest server version:', err);
      });

    return version;
  };

  return {
    serverVersion,
    latestServerVersion,
    featuresCompatibility,
    initServerVersion,
  };
};
