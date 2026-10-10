import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { HttpError } from '@hasura/shared/types';
import {
  ServerJsonRootConfig,
  TemplateGallerySection,
  TemplateGalleryTemplateItem,
  ROOT_CONFIG_PATH,
} from '../types';
import { request } from '@hasura/shared/utils';

type Options<T = TemplateGallerySection[]> = UseQueryOptions<
  T,
  HttpError,
  TemplateGallerySection[]
>;

const GLOBAL_SCHEMA_SHARING_CONFIGURATION_QUERY_KEY =
  'GLOBAL_SCHEMA_SHARING_CONFIGURATION';

export const useGlobalSchemaSharingConfiguration = (options?: Options) => {
  return useQuery({
    queryKey: [GLOBAL_SCHEMA_SHARING_CONFIGURATION_QUERY_KEY],
    queryFn: async () => {
      const data = await request(ROOT_CONFIG_PATH).then((response) =>
        response.json(),
      );

      return mapRootJsonFromServerToState(data);
    },
    staleTime: Infinity,
    ...options,
  });
};

const mapRootJsonFromServerToState = (
  data: ServerJsonRootConfig,
): TemplateGallerySection[] => {
  const sectionsGroups: Record<string, TemplateGalleryTemplateItem[]> =
    Object.entries(data)
      .map(([key, value]) => ({
        ...value,
        key,
      }))
      .filter(
        (value) =>
          value.metadata_version === '3' && value.template_version === '1',
      )
      .reduce<Record<string, TemplateGalleryTemplateItem[]>>(
        (previousValue, currentValue) => {
          const item: TemplateGalleryTemplateItem = {
            type: 'database',
            key: currentValue.key,
            description: currentValue.description,
            dialect: currentValue.dialect,
            title: currentValue.title,
            relativeFolderPath: currentValue.relativeFolderPath,
            metadataVersion: +currentValue.metadata_version,
            templateVersion: +currentValue.template_version,
          };

          return {
            ...previousValue,
            [currentValue.category]: [
              ...(previousValue[currentValue.category] ?? []),
              item,
            ],
          };
        },
        {},
      );

  const sections: TemplateGallerySection[] = Object.entries(sectionsGroups).map(
    ([name, templates]) => ({
      name,
      templates,
    }),
  );

  return sections;
};
