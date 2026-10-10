import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { HttpError } from '@hasura/shared/types';
import {
  TemplateGalleryTemplateItem,
  TemplateGalleryTemplateDetailFull,
  BASE_URL_TEMPLATE,
  BASE_URL_PUBLIC,
  ServerJsonTemplateDefinition,
} from '../types';
import { request } from '@hasura/shared/utils';
import { Metadata } from '@hasura/shared/types';

type UseSchemaConfigurationByNameProps = {
  template: TemplateGalleryTemplateItem;
};

type Options<T = TemplateGalleryTemplateDetailFull> = UseQueryOptions<
  T,
  HttpError,
  TemplateGalleryTemplateDetailFull
>;

const TEMPLATE_SCHEMA_CONFIGURATION_BY_NAME_QUERY_KEY =
  'TEMPLATE_SCHEMA_CONFIGURATION_BY_NAME';

export const useSchemaConfigurationByName = (
  { template }: UseSchemaConfigurationByNameProps,
  options?: Options,
) => {
  return useQuery({
    queryKey: [TEMPLATE_SCHEMA_CONFIGURATION_BY_NAME_QUERY_KEY, template.key],
    queryFn: async () => {
      const baseTemplatePath = `${BASE_URL_TEMPLATE}/${template.relativeFolderPath}`;
      const publicUrl = `${BASE_URL_PUBLIC}/${template.relativeFolderPath}`;

      const itemConfig: ServerJsonTemplateDefinition = await request(
        `${baseTemplatePath}/config.json`,
      ).then((response) => response.json());

      const sqlFiles = await Promise.all(
        itemConfig.sqlFiles.map((sqlFile) =>
          request(`${baseTemplatePath}/${sqlFile}`).then((response) =>
            response.text(),
          ),
        ),
      );

      const metadataObject: Metadata = await request(
        `${baseTemplatePath}/${itemConfig.metadataUrl}`,
      ).then((response) => response.json());

      const fullObject: TemplateGalleryTemplateDetailFull = {
        sql: sqlFiles.join('\n'),
        blogPostLink: itemConfig.blogPostLink,
        imageUrl: itemConfig.imageUrl
          ? `${baseTemplatePath}/${itemConfig.imageUrl}`
          : undefined,
        longDescription: itemConfig.longDescription,
        metadataObject,
        publicUrl,
      };

      return fullObject;
    },
    staleTime: Infinity,
    ...options,
  });
};
