import { GraphQLCustomizationSchema } from '../schema';

export const generateGraphQLCustomizationInfo = (
  values: GraphQLCustomizationSchema
) => {
  return {
    root_fields: {
      namespace: values?.rootFields?.namespace,
      prefix: values?.rootFields?.prefix,
      suffix: values?.rootFields?.suffix,
    },
    type_names: {
      prefix: values?.typeNames?.prefix,
      suffix: values?.typeNames?.suffix,
    },
    naming_convention: values?.namingConvention,
  };
};
