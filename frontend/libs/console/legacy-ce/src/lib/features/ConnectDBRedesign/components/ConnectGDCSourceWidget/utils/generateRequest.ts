import { DatabaseConnection } from '@/features/ConnectDBRedesign/types';
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
      configuration: values.configuration,
      customization: generateGraphQLCustomizationInfo(
        values.customization ?? {}
      ),
    },
  };
  return cleanEmpty(payload);
};
