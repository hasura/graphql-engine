import { SupportedDriver } from '@hasura/shared/types';
import { TemplateGallerySection } from './types';

export const getSchemasForDb = (
  templates: TemplateGallerySection[],
  driver: SupportedDriver,
) =>
  templates
    .map((section) => ({
      ...section,
      templates: section.templates.filter(
        (template) => template.dialect === driver,
      ),
    }))
    .filter((section) => section.templates.length > 0);
