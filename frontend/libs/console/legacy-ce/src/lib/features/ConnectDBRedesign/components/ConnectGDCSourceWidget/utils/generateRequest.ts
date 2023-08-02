import { DatabaseConnection } from '../../../types';
import { cleanEmpty } from '../../ConnectPostgresWidget/utils/helpers';
import { generateGraphQLCustomizationInfo } from '../../GraphQLCustomization';

export const generateGDCRequestPayload = ({
  driver,
  values,
}: {
  driver: string;
  values: any;
}): DatabaseConnection => {
  const payload = {
    driver,
    details: {
      name: values.name,
      configuration: {
        value: values.configuration,
        timeout: { seconds: values.timeout },
        template: values.template,
      },
      customization: generateGraphQLCustomizationInfo(
        values.customization ?? {}
      ),
    },
  };
  return cleanEmpty(payload);
};
