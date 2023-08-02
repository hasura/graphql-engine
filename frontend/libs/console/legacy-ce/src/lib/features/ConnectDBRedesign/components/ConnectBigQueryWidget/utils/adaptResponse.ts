import {
  BigQueryConfiguration,
  Source,
} from '../../../../hasura-metadata-types';
import isArray from 'lodash/isArray';
import { adaptGraphQLCustomization } from '../../GraphQLCustomization/utils/adaptResponse';
import { BigQueryConnectionSchema } from '../schema';

export const adaptPostgresConnection = (
  metadataSource: Source
): BigQueryConnectionSchema => {
  if (metadataSource.kind !== 'bigquery')
    throw Error('Not a bigquery connection');

  // This assertion is safe because of the check above.
  const configuration = metadataSource.configuration as BigQueryConfiguration;

  return {
    name: metadataSource.name,
    configuration: {
      serviceAccount:
        'from_env' in configuration.service_account
          ? {
              type: 'envVar',
              envVar: configuration.service_account.from_env,
            }
          : {
              type: 'serviceAccountKey',
              value: configuration.service_account,
            },
      projectId:
        typeof configuration.project_id === 'string'
          ? { type: 'value', value: configuration.project_id }
          : { type: 'envVar', envVar: configuration.project_id.from_env },
      datasets: isArray(configuration.datasets)
        ? { type: 'value', value: configuration.datasets.join() }
        : { type: 'envVar', envVar: configuration.datasets.from_env },
    },
    customization: adaptGraphQLCustomization(
      metadataSource.customization ?? {}
    ),
  };
};
