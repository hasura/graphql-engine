import { DeepRequired } from 'ts-essentials';
import { DataSourcesAPI } from '../..';
import { SupportedFeaturesType } from '../../types';
import {
  postgres,
  supportedFeatures as PgSupportedFeatures,
} from '../postgresql';

export const supportedFeatures: DeepRequired<SupportedFeaturesType> = {
  ...PgSupportedFeatures,
  driver: {
    ...PgSupportedFeatures?.driver,
    name: 'alloy',
  },
};

export const alloy: DataSourcesAPI = {
  ...postgres,
};
