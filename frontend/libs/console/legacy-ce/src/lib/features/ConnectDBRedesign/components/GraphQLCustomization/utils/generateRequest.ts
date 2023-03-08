import { GraphQLCustomization } from '../schema';

export const generateGraphQLCustomizationInfo = (
  values: GraphQLCustomization
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
    namingConvention: values?.namingConvention,
  };
};
