import { useMetadataSource } from '../../MetadataAPI';

interface Args {
  name: string;
  driver: string;
}

export const useExistingConfig = (name: string) => {
  const { data, ...rest } = useMetadataSource(name);
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
