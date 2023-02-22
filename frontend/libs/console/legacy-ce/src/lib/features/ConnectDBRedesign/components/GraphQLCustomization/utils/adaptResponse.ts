import { SourceCustomization } from '../../../../hasura-metadata-types';
import { GraphQLCustomization } from '../schema';

export const adaptGraphQLCustomization = (
  sourceCustomization: SourceCustomization
): GraphQLCustomization => {
  return {
    rootFields: {
      namespace: sourceCustomization.root_fields?.namespace,
      prefix: sourceCustomization.root_fields?.prefix,
      suffix: sourceCustomization.root_fields?.suffix,
    },
    typeNames: {
      prefix: sourceCustomization.type_names?.prefix,
      suffix: sourceCustomization.type_names?.suffix,
    },
    namingConvention: sourceCustomization.naming_convention,
  };
};
