import { DatabaseConnection } from '../../../types';
import { generateGraphQLCustomizationInfo } from '../../GraphQLCustomization/utils/generateRequest';
import { BigQueryConnectionSchema } from '../schema';
import { cleanEmpty } from '../../ConnectPostgresWidget/utils/helpers';

export const generateBigQueryRequestPayload = ({
  driver,
  values,
}: {
  driver: string;
  values: BigQueryConnectionSchema;
}): DatabaseConnection => {
  const payload = {
    driver,
    details: {
      name: values.name,
      configuration: {
        service_account:
          values.configuration.serviceAccount.type === 'envVar'
            ? { from_env: values.configuration.serviceAccount.envVar }
            : values.configuration.serviceAccount.value,
        project_id:
          values.configuration.projectId.type === 'envVar'
            ? { from_env: values.configuration.projectId.envVar }
            : values.configuration.projectId.value,
        datasets:
          values.configuration.datasets.type === 'envVar'
            ? { from_env: values.configuration.datasets.envVar }
            : values.configuration.datasets.value.split(','),
      },
      customization: generateGraphQLCustomizationInfo(
        values.customization ?? {}
      ),
    },
  };
  return cleanEmpty(payload);
};
