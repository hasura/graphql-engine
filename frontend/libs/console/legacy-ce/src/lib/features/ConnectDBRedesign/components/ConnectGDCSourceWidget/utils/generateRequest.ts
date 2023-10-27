import { DatabaseConnection } from '../../../types';
import { cleanEmpty } from '../../ConnectPostgresWidget/utils/helpers';
import { generateGraphQLCustomizationInfo } from '../../GraphQLCustomization';
import { templateVariableArrayToMap } from '../useFormValidationSchema';

export const generateGDCRequestPayload = ({
  driver,
  values,
}: {
  driver: string;
  values: any;
}): DatabaseConnection => {
  return cleanEmpty({
    driver,
    details: {
      name: values.name,
      configuration: {
        value: values.configuration,
        timeout: { seconds: values.timeout },
        template: values.template,
        template_variables: templateVariableArrayToMap(
          values?.template_variables || []
        ),
      },
      customization: generateGraphQLCustomizationInfo(
        values.customization ?? {}
      ),
    },
  });
};
