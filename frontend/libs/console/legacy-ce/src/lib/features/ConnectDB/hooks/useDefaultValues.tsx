import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';

interface Args {
  name: string;
  driver: string;
}

export const useExistingConfig = (name: string) => {
  const { data, ...rest } = useMetadata(MetadataSelectors.findSource(name));
  return { data: data?.configuration, ...rest };
};

export const useDefaultValues = ({ name, driver }: Args) => {
  const { data: configuration, ...rest } = useExistingConfig(name);

  return {
    data: {
      name,
      driver,
      configuration,
    },
    ...rest,
  };
};
