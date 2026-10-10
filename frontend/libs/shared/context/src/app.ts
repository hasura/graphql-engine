import { createContext, useContext } from 'react';
import { FeaturesCompatibility } from '@hasura/shared/utils';
import { LatestReleaseVersionResult, EnvVars } from '@hasura/shared/types';
import { Endpoints } from './endpoints';

export type AppState = {
  readOnlyMode: boolean;
  serverVersion: string;
  isProduction: boolean;
  latestServerVersion: LatestReleaseVersionResult;
  featuresCompatibility: FeaturesCompatibility;
  envVars: EnvVars;
  endpoints: Endpoints;
};

export const defaultAppState: AppState = {
  readOnlyMode: false,
  serverVersion: '',
  latestServerVersion: {
    latest: '',
    prerelease: '',
  },
  featuresCompatibility: {},
  envVars: {} as EnvVars,
  endpoints: {} as Endpoints,
  isProduction: false,
};

export const AppContext = createContext(defaultAppState);

export const useAppContext = () => useContext(AppContext);
